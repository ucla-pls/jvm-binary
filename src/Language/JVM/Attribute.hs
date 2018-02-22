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
  , Const
  -- , matching
  , firstOne
  ) where


-- import           Data.Foldable
import           Data.List as List
import           Data.Bifunctor
-- import           Data.Maybe
import qualified Data.ByteString.Lazy                    as BL
import           Data.Binary

import qualified Data.Text as Text

import           Language.JVM.Stage
-- import           Language.JVM.Constant
import           Language.JVM.Utils                      (trd)

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

-- -- | Return a list of either parsed attributes or error messages from an object
-- -- given a function that object to a collection of attributes. The list only
-- -- contains matching attributes.
-- matching
--   :: (IsAttribute a, Traversable t)
--   => (b -> t Attribute)
--   -> b
--   -> PoolAccess [Either String a]
-- matching fn b = do
--   catMaybes . toList <$> sequence (fromAttribute' <$> fn b)

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

-- -- | 'Code' is an Attribute.
-- instance IsAttribute Code where
--   attrName = Const "Code"


-- -- | 'Exceptions' is an Attribute.
-- instance IsAttribute Exceptions where
--   attrName = Const "Exceptions"

-- -- | 'StackMapTable' is an Attribute.
-- instance IsAttribute StackMapTable where
--   attrName = Const "StackMapTable"

-- -- | 'BootstrapMethods' is an Attribute.
-- instance IsAttribute BootstrapMethods where
--   attrName = Const "BootstrapMethods"

-- | Maybe return the first element of a list
firstOne :: [a] -> Maybe a
firstOne as = fst <$> List.uncons as

-- -- | The a attribute of the code is the StackMapTable. There can be at most one.
-- -- If the version number of the file is more than 50, and there is no StackMapTable.
-- -- there is an implicit empty StackMapTable.
-- codeStackMapTable :: Code -> PoolAccess (Maybe (Either String BootstrapMethods))
-- codeStackMapTable c = do
--   firstOne <$> matching Code.codeAttributes c
