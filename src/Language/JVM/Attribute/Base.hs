{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ExistentialQuantification       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-|
Module      : Language.JVM.Attribute.Base
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}
module Language.JVM.Attribute.Base
  ( Attribute (..)
  , aInfo
  , toAttribute
  , toBCAttribute
  , devolveAttribute
  , fromAttribute'
  , toAttribute'

  -- * Helpers
  , IsAttribute (..)
  , Attributes
  , fromAttributes
  , collect
  , collectBC
  , AttributeCollector (..)
  , ByteCodeAttributeCollector (..)
  , firstOne

  -- * re-export
  , Const (..)
  ) where

-- base
import Data.Monoid
import           Control.Monad
import           Control.Applicative
import           Data.Maybe
import qualified Data.List            as List

-- binary
import           Data.Binary

-- bytestring
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

-- text
import qualified Data.Text            as Text

import           Language.JVM.Staged
import           Language.JVM.ByteCode
import           Language.JVM.Utils

-- | Maybe return the first element of a list
firstOne :: [a] -> Maybe a
firstOne as = fst <$> List.uncons as

-- | An Attribute, simply contains of a reference to a name and
-- contains info.
data Attribute r = Attribute
  { aName  :: ! (Ref Text.Text r)
  , aInfo' :: ! SizedByteString32
  }

-- | A small helper function to extract the info as a
-- lazy 'Data.ByteString.Lazy.ByteString'.
aInfo :: Attribute r -> BS.ByteString
aInfo = unSizedByteString . aInfo'

instance Staged Attribute where
  evolve (Attribute an ai) = do
    an' <- link an
    return $ Attribute an' ai
  devolve (Attribute an ai) = do
    an' <- unlink an
    return $ Attribute an' ai

$(deriveBaseWithBinary ''Attribute)

-- | A list of attributes and described by the expected values.
type Attributes b r = Choice (SizedList16 (Attribute r)) (b r) r

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
  let name = getConst (attrName :: Const Text.Text a)
      bytes = encode a
  in Attribute name (SizedByteString . BL.toStrict $ bytes)

toAttribute :: (IsAttribute (a Low), Staged a, DevolveM m) => a High -> m (Attribute Low)
toAttribute =
  devolveAttribute devolve

toBCAttribute ::
  (IsAttribute (a Low), ByteCodeStaged a, DevolveM m)
  => (ByteCodeIndex -> m ByteCodeOffset)
  -> a High
  -> m (Attribute Low)
toBCAttribute bcde =
  devolveAttribute (devolveBC bcde)

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
  if aName as == getConst (attrName :: Const Text.Text (a Low))
  then Just . label (Text.unpack $ aName as) . either evolveError evolve $ fromAttribute' as
  else Nothing

-- | Generate an BCAttribute in the 'EvolveM' monad
fromBCAttribute ::
  forall a m. (IsAttribute (a Low), ByteCodeStaged a, EvolveM m)
  => (ByteCodeOffset -> m ByteCodeIndex)
  -> Attribute High
  -> Maybe (m (a High))
fromBCAttribute fn as =
  if aName as == getConst (attrName :: Const Text.Text (a Low))
  then Just . label (Text.unpack $ aName as) . either evolveError (evolveBC fn) $ fromAttribute' as
  else Nothing

-- -- | Generate an attribute in the 'EvolveM' monad
-- evolveAttribute ::
--   forall a m. (IsAttribute (a Low), EvolveM m)
--   => (a Low -> m (a High))
--   -> Attribute High
--   -> Maybe (m (a High))
-- evolveAttribute g as =
--   if aName as == getConst (attrName :: Const Text.Text (a Low))
--   then Just $ either attributeError g $ fromAttribute' as
--   else Nothing

collect ::
  forall c m. (EvolveM m)
  => [AttributeCollector c]
  -> (Attribute High -> c -> c)
  -> Attribute High
  -> m (Endo c)
collect options def attr =
  fromMaybe (return $ Endo (def attr))
  . msum
  $ (\(Attr fn) -> fmap (Endo . fn) <$> fromAttribute attr) <$> options

data AttributeCollector c
  = forall a. (IsAttribute (a Low), Staged a)
    => Attr (a High -> c -> c)

collectBC ::
  forall c m. (EvolveM m)
  => (ByteCodeOffset -> m ByteCodeIndex)
  -> [ByteCodeAttributeCollector c]
  -> (Attribute High -> c -> c)
  -> Attribute High
  -> m (Endo c)
collectBC evolvefn options def attr =
  fromMaybe (return $ Endo (def attr))
  . msum
  $ (\(BCAttr fn) -> fmap (Endo . fn) <$> fromBCAttribute evolvefn attr) <$> options

data ByteCodeAttributeCollector c
  = forall a. (IsAttribute (a Low), ByteCodeStaged a)
    => BCAttr (a High -> c -> c)


-- | Given a 'Foldable' structure 'f', and a function that can calculate a
-- monoid given an 'Attribute' calculate the monoid over all attributes.
fromAttributes ::
  (Foldable f, EvolveM m, Monoid a)
  => AttributeLocation
  -> f (Attribute Low)
  -> (Attribute High -> m a)
  -> m a
fromAttributes al attrs f = do
  afilter <- attributeFilter
  Prelude.foldl (g afilter) (return mempty) attrs
  where
    g afilter m a' = do
      b <- m
      ah <- evolve a'
      if afilter (al, aName ah)
        then do
          x <- f ah
          return $ b `mappend` x
      else return b

readFromStrict :: Binary a => Attribute r -> Either String a
readFromStrict =
    either (Left . trd) tst . decodeOrFail . BL.fromStrict . aInfo
    where
      tst :: (BL.ByteString, ByteOffset, a) -> Either String a
      tst (s, _, a) = 
        if BL.null s then Right a else Left "Incomplete attribute parsing"
