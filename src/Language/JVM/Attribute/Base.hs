{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Language.JVM.Attribute.Base
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}
module Language.JVM.Attribute.Base
  ( Attribute (..)
  , aInfo
  , aName

  -- * Helpers
  , IsAttribute (..)
  , fromAttributes
  , toC
  , collect
  , Const (..)
  , firstOne
  ) where

import qualified Data.Text             as Text
import qualified Data.List             as List
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.Binary
import Data.Bifunctor
import Control.Monad

import           Language.JVM.Stage
import           Language.JVM.Constant
import           Language.JVM.Utils


-- | Maybe return the first element of a list
firstOne :: [a] -> Maybe a
firstOne as = fst <$> List.uncons as

-- | An Attribute, simply contains of a reference to a name and
-- contains info.
data Attribute r = Attribute
  { aNameIndex :: ! (Ref Text.Text r)
  , aInfo'     :: ! (SizedByteString32)
  }

-- | A small helper function to extract the info as a
-- lazy 'Data.ByteString.Lazy.ByteString'.
aInfo :: Attribute r -> BS.ByteString
aInfo = unSizedByteString . aInfo'

-- | Extracts the name from the attribute, if it exists in the
-- ConstantPool.
aName :: Attribute High -> Text.Text
aName = valueF aNameIndex

instance Staged Attribute where
  evolve (Attribute an ai) = do
    an' <- evolve an
    return $ Attribute an' ai
  devolve (Attribute an ai) = do
    an' <- devolve an
    return $ Attribute an' ai

$(deriveBaseWithBinary ''Attribute)


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
  case msum $ Prelude.map ($ attr) options of
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
  Prelude.foldl g (return mempty) attrs
  where
    g m a' = do
      b <- m
      x <- f =<< evolve a'
      return $ b `mappend` x

readFromStrict :: Binary a => Attribute r -> Either String a
readFromStrict =
    bimap trd trd . decodeOrFail . BL.fromStrict . aInfo
