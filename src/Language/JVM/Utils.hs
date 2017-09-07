{-# LANGUAGE DeriveFunctor #-}
module Language.JVM.Utils
  ( SizedList16 (..)
  , SizedByteString32 (..)

  , trd
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Control.Monad

import qualified Data.ByteString as BS

newtype SizedList16 a = SizedList16
  { unSizedList16 :: [ a ]
  } deriving (Show, Eq, Functor)

instance Foldable SizedList16 where
  foldMap am =
    foldMap am . unSizedList16

instance Traversable SizedList16 where
  traverse afb ta =
    SizedList16 <$> traverse afb (unSizedList16 ta)

instance Binary a => Binary (SizedList16 a) where
  get = do
    len <- getInt16be
    SizedList16 <$> replicateM (fromIntegral len) get

  put (SizedList16 sl) = do
    putInt16be (fromIntegral $ length sl)
    forM_ sl put

newtype SizedByteString32 = SizedByteString32
  { unSizedByteString32 :: BS.ByteString
  } deriving (Show, Eq)

instance Binary SizedByteString32 where
  get = do
    x <- getWord32be
    SizedByteString32 <$> getByteString (fromIntegral x)
  put (SizedByteString32 bs) = do
    putWord32be (fromIntegral . BS.length $ bs)
    putByteString bs

-- | Takes the third element of a triple.
trd :: (a, b, c) -> c
trd (_, _, c) = c
