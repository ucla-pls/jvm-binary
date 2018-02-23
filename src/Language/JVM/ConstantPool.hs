{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-|
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'ConstantPool' data structure and multiple
other types, and classes.
-}
module Language.JVM.ConstantPool
  (
  -- * Constant Pool
  -- $ConstantPool
    ConstantPool (..)
  , access
  , append
  , empty
  , PoolAccessError (..)
  ) where

import           Control.DeepSeq          (NFData)
import           Control.Monad.Except
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.IntMap              as IM
import           GHC.Generics             (Generic)

import           Language.JVM.Constant
import           Language.JVM.TH


-- $ConstantPool
-- The 'ConstantPool' contains all the constants, and is accessible using the
-- Lookup methods.

-- | A ConstantPool is just an 'IntMap'. A 'IntMap' is used, because constants are
-- accessed using their byte-offset, and sometimes the offset depends on the constant
-- size. See 'constantSize'.
newtype ConstantPool r = ConstantPool
  { unConstantPool :: IM.IntMap (Constant r)
  }

instance Binary (ConstantPool Low) where
  get = do
    len <- fromIntegral <$> getInt16be
    list <- go len 1
    return . ConstantPool $ IM.fromList list
    where
      go len n | len > n = do
        constant <- get
        rest <- go len (n + constantSize constant)
        return $ (n, constant) : rest
      go _ _ = return []
  put (ConstantPool p) = do
    case IM.maxViewWithKey p of
      Just ((key, e), _) -> do
        putInt16be (fromIntegral (key + constantSize e))
        forM_ (IM.toAscList p) (put . snd)
      Nothing -> do
        putInt16be 0

-- | A pool access error
data PoolAccessError = PoolAccessError
  { paErrorRef :: !Word16
  , paErrorMsg :: String
  } deriving (Show, Eq, Generic)

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

-- | Append a constant to the constant pool, and get the offset.
append :: Constant r -> ConstantPool r -> (Index, ConstantPool r)
append c (ConstantPool cp) =
  (fromIntegral i, ConstantPool $ IM.insert i c cp)
  where
    i =
      case IM.toDescList cp of
        (k, a):_ ->
          k + constantSize a
        _ -> 0

$(deriveBase ''ConstantPool)
