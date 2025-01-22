{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'ConstantPool' data structure and multiple
other types, and classes.
-}
module Language.JVM.ConstantPool (
  -- * Constant Pool
  -- $ConstantPool
  ConstantPool (..),
  access,
  growPool,
  poolCount,
  nextIndex,
  listConstants,
  fromConstants,
  empty,
  PoolAccessError (..),
  Index,
) where

import Control.DeepSeq (NFData)
import Data.Binary

import Data.Binary.Get
import Data.Binary.Put

-- import Debug.Trace
import GHC.Generics (Generic)

-- base
import Data.Monoid
import Data.Foldable

-- containers
import qualified Data.IntMap as IM

import Language.JVM.Constant
import Language.JVM.Stage
import Language.JVM.TH

{- $ConstantPool
 The 'ConstantPool' contains all the constants, and is accessible using the
 Lookup methods.
-}

{- | A ConstantPool is just an 'IntMap'. A 'IntMap' is used, because constants are
 accessed using their byte-offset, and sometimes the offset depends on the constant
 size. See 'constantSize'.
-}
newtype ConstantPool r = ConstantPool
  { unConstantPool :: IM.IntMap (Constant r)
  }

instance Binary (ConstantPool Low) where
  get = do
    len <- fromIntegral <$> getWord16be
    list <- go len 1
    return . ConstantPool $ IM.fromList list
   where
    go len n | len > n = do
      constant <- label "constant" $ get
      rest <- go len (n + constantSize constant)
      return $ (fromIntegral n, constant) : rest
    go _ _ = return []
  put (ConstantPool p) = do
    case IM.maxViewWithKey p of
      Just ((key, e), _) -> do
        putWord16be (fromIntegral key + constantSize e)
        forM_ (IM.toAscList p) (put . snd)
      Nothing -> do
        putInt16be 0

-- | A pool access error
data PoolAccessError = PoolAccessError
  { paErrorRef :: !Word16
  , paErrorMsg :: String
  }
  deriving (Show, Eq, Generic)

instance NFData PoolAccessError

-- | Creates an empty constant pool
empty :: ConstantPool r
empty = ConstantPool (IM.empty)

-- | Access a constant in the constant pool
access :: Index -> ConstantPool r -> Either PoolAccessError (Constant r)
access ref (ConstantPool cp) =
  case IM.lookup (fromIntegral ref) cp of
    Just x -> Right x
    Nothing -> Left $ PoolAccessError ref "No such element."

poolCount :: ConstantPool r -> Int
poolCount =
  IM.size . unConstantPool

listConstants :: ConstantPool r -> [(Index, Constant r)]
listConstants =
  map (\(i, a) -> (fromIntegral i, a)) . IM.toList . unConstantPool

nextIndex :: ConstantPool r -> Index
nextIndex (ConstantPool im) =
  fromIntegral $ case IM.toDescList im of
    (k, a) : _ -> fromIntegral k + constantSize a
    _ -> 1

fromConstants :: Foldable f => f (Constant r) -> ConstantPool r
fromConstants =
  foldl (\b a -> snd . append a $ b) empty

growPool
  :: forall b
   . (ConstantPool High -> Constant Low -> Either b (Constant High))
  -> ConstantPool Low
  -> (ConstantPool High, [(b, (Index, Constant Low))])
growPool f reffed =
  stage' IM.empty (listConstants reffed)
 where
  stage' :: IM.IntMap (Constant High) -> [(Index, Constant Low)] -> (ConstantPool High, [(b, (Index, Constant Low))])
  stage' cp mis =
    case foldMap (grow (ConstantPool cp)) mis of
      (cp', flip appEndo [] -> mis')
        | IM.null cp' ->
            (ConstantPool cp, mis')
        | otherwise ->
            stage' (cp `IM.union` cp') . map snd $ mis'

  grow cp (k, a) =
    case f cp a of
      Right c -> (IM.singleton (fromIntegral k) c, mempty)
      Left b -> (IM.empty, Endo ((b, (k, a)) :))
{-# INLINE growPool #-}

-- | Append a constant to the constant pool, and get the offset.
append :: Constant r -> ConstantPool r -> (Index, ConstantPool r)
append c cp@(ConstantPool im) =
  (i, ConstantPool $ IM.insert (fromIntegral i) c im)
 where
  i = nextIndex cp

$(deriveBase ''ConstantPool)
