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
  , toAttribute
  , devolveAttribute

  -- * Helpers
  , IsAttribute (..)
  , fromAttributes
  , toC
  , toC'
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

import           Language.JVM.Staged
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
aName = value . aNameIndex

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
class (Binary a) => IsAttribute a where
  -- | The name of an attribute. This is used to lookup an attribute.
  attrName :: Const Text.Text a

-- | Generate an attribute in a low stage 'Low'.
fromAttribute' :: IsAttribute a => Attribute r -> Either String a
fromAttribute' = readFromStrict

toAttribute' :: forall a. IsAttribute a => a -> Attribute High
toAttribute' a =
  let name = unConst (attrName :: Const Text.Text a)
      bytes = encode a
  in Attribute (RefV name) (SizedByteString . BL.toStrict $ bytes)

toAttribute :: (IsAttribute (a Low), Staged a, DevolveM m) => a High -> m (Attribute Low)
toAttribute =
  devolveAttribute devolve

devolveAttribute :: (IsAttribute (a Low), DevolveM m) => (a High -> m (a Low)) -> a High -> m (Attribute Low)
devolveAttribute f a = do
  a' <- f a
  devolve $ toAttribute' a'

-- | Generate an attribute in the 'EvolveM' monad
fromAttribute ::
  forall a m. (IsAttribute (a Low), Staged a, EvolveM m)
  => Attribute High
  -> Maybe (m (a High))
fromAttribute as =
  if aName as == unConst (attrName :: Const Text.Text (a Low))
  then Just $ do
    either attributeError evolve $ fromAttribute' as
  else Nothing

-- | Generate an attribute in the 'EvolveM' monad
evolveAttribute ::
  forall a m. (IsAttribute (a Low), EvolveM m)
  => (a Low -> m (a High))
  -> Attribute High
  -> Maybe (m (a High))
evolveAttribute g as =
  if aName as == unConst (attrName :: Const Text.Text (a Low))
  then Just $ do
    either attributeError g $ fromAttribute' as
  else Nothing

toC :: (EvolveM m, Staged a, IsAttribute (a Low)) => (a High -> c) -> Attribute High -> Maybe (m c)
toC f attr =
  case fromAttribute attr of
    Just m -> Just $ f <$> m
    Nothing -> Nothing

toC' :: (EvolveM m, IsAttribute (a Low)) => (a Low -> m (a High)) -> (a High -> c) -> Attribute High -> Maybe (m c)
toC' g f attr =
  case evolveAttribute g attr of
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
