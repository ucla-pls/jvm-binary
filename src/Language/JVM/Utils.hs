{-# LANGUAGE DeriveFunctor #-}
module Language.JVM.Utils
  ( SizedList16 (..)
  , SizedByteString32 (..)

  , BitSet16 (..)

  , Enumish(..)
  , trd
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.List       as List
import           Data.Set        as Set

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

-- | An Enumish value, all maps to a number, but
-- not all integers maps to a enumsish value. There is
-- no guarantee that the integers will be subsequent.
class (Eq a, Ord a) => Enumish a where
  inOrder :: [(Int, a)]
  fromEnumish :: a -> Int
  fromEnumish a = let Just (i, _) = List.find ((== a) . snd) $ inOrder in i
  toEnumish :: Int -> Maybe a
  toEnumish i = snd <$> (List.find ((== i) . fst) $ inOrder)

newtype BitSet16 a = BitSet16 { toSet :: Set.Set a }
  deriving (Ord, Show, Eq)

instance (Enumish a) => Binary (BitSet16 a) where
  get = do
    word <- getWord16be
    return . BitSet16 $ Set.fromList [ x | (i, x) <- inOrder, testBit word i ]

  put (BitSet16 f) = do
    let word = List.foldl' setBit zeroBits (List.map fromEnumish $ Set.toList f)
    putWord16be word

-- | Takes the third element of a triple.
trd :: (a, b, c) -> c
trd (_, _, c) = c
