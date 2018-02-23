{-|
Module      : Language.JVM.Attribute
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This is the main module for accessing all kinds of Attributes.
-}

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.JVM.Attribute
  (
   module Language.JVM.Attribute.Base

  -- * SubAttributes
  , IsAttribute (..)
  , Code
  --, codeStackMapTable
  , ConstantValue
  , Exceptions
  , StackMapTable
  , BootstrapMethods

  -- * Helpers
  , fromAttributes
  , toC
  , collect
  , Const
  -- , matching
  , firstOne
  ) where


-- import           Data.Foldable
import           Data.List as List
import           Data.Bifunctor
import           Control.Monad
-- import           Data.Maybe
import qualified Data.ByteString.Lazy                    as BL
import           Data.Binary

import qualified Data.Text as Text

import           Language.JVM.Stage
import           Language.JVM.Constant
import           Language.JVM.Utils

import           Language.JVM.Attribute.BootstrapMethods (BootstrapMethods)
import Language.JVM.Attribute.Code (Code)
import           Language.JVM.Attribute.ConstantValue    (ConstantValue)
import           Language.JVM.Attribute.Exceptions       (Exceptions)
import           Language.JVM.Attribute.StackMapTable    (StackMapTable)
import           Language.JVM.Attribute.Base


-- | Create a type dependent on another type 'b',
-- used for accessing the correct 'attrName' in 'IsAttribute'.
newtype Const a b = Const { unConst :: a }

-- | A class-type that describes a data-type 'a' as an Attribute. Most notable
-- it provides the 'fromAttribute'' method that enables converting an Attribute
-- to a data-type 'a'.
class (Binary (a Low), Staged a) => IsAttribute a where
  -- | The name of an attribute. This is used to lookup an attribute.
  attrName :: Const Text.Text (a Low)

  -- | Generate an attribute in a low stage 'Low'.
  fromAttribute' :: Attribute r -> Either String (a Low)
  fromAttribute' = readFromStrict

  -- | Generate an attribute in the 'EvolveM' monad
  fromAttribute :: EvolveM m => Attribute High -> Maybe (m (a High))
  fromAttribute as =
    if aName as == unConst (attrName :: Const Text.Text (a Low))
    then Just $ do
      either attributeError evolve $ fromAttribute' as
    else Nothing

  toAttribute' :: a Low -> Attribute High
  toAttribute' a =
    let name = unConst (attrName :: Const Text.Text (a Low))
        bytes = encode a
    in Attribute (RefV name) (SizedByteString . BL.toStrict $ bytes)

  toAttribute :: DevolveM m => a High -> m (Attribute Low)
  toAttribute a = do
    a' <- devolve a
    devolve $ toAttribute' a'

  -- toAttribute :: DevolveM m => a High -> m (Attribute Low)
  -- toAttribute

toC :: (EvolveM m, IsAttribute a) => (a High -> c) -> Attribute High -> Maybe (m c)
toC f attr =
  case fromAttribute attr of
    Just m -> Just $ f <$> m
    Nothing -> Nothing

collect :: (Monad m) => c -> Attribute High -> [Attribute High -> Maybe (m c)] -> m c
collect c attr options =
  case msum $ map ($ attr) options of
    Just x -> x
    Nothing -> return c

-- | Given a 'Foldable' structure 'f', and a function that can calculate a
-- monoid given an 'Attribute' calculate the monoid over all attributes.
fromAttributes ::
  (Foldable f, EvolveM m, Monoid a)
  => (Attribute High -> m a)
  -> f (Attribute Low)
  -> m a
fromAttributes f attrs =
  foldl g (return mempty) attrs
  where
    g m a' = do
      b <- m
      x <- f =<< evolve a'
      return $ b `mappend` x

readFromStrict :: Binary a => Attribute r -> Either String a
readFromStrict =
    bimap trd trd . decodeOrFail . BL.fromStrict . aInfo


-- # Attributes

-- | 'ConstantValue' is an Attribute.
instance IsAttribute ConstantValue where
  attrName = Const "ConstantValue"

-- | 'Code' is an Attribute.
instance IsAttribute Code where
  attrName = Const "Code"

-- | 'Exceptions' is an Attribute.
instance IsAttribute Exceptions where
  attrName = Const "Exceptions"

-- | 'StackMapTable' is an Attribute.
instance IsAttribute StackMapTable where
  attrName = Const "StackMapTable"

-- | 'BootstrapMethods' is an Attribute.
instance IsAttribute BootstrapMethods where
  attrName = Const "BootstrapMethods"

-- | Maybe return the first element of a list
firstOne :: [a] -> Maybe a
firstOne as = fst <$> List.uncons as

-- -- | The a attribute of the code is the StackMapTable. There can be at most one.
-- -- If the version number of the file is more than 50, and there is no StackMapTable.
-- -- there is an implicit empty StackMapTable.
-- codeStackMapTable :: Code -> PoolAccess (Maybe (Either String BootstrapMethods))
-- codeStackMapTable c = do
--   firstOne <$> matching Code.codeAttributes c
