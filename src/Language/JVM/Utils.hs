{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Language.JVM.Utils
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains utilities missing not in other libraries.
-}
module Language.JVM.Utils (
  -- * Sized Data Structures

  --
  -- $SizedDataStructures
  SizedList (..),
  listSize,
  SizedByteString (..),
  byteStringSize,

  -- ** Specific sizes
  SizedList8,
  SizedList16,
  SizedByteString32,
  SizedByteString16,
  sizedByteStringFromText,
  sizedByteStringToText,
  tryDecode,

  -- * Bit Set

  --
  -- $BitSet
  BitSet (..),
  Enumish (..),

  -- ** Specific sizes
  BitSet16,

  -- * General Utilities
  -- $utils
  trd,
) where

-- binary
import Data.Binary
import Data.Binary.Get as Get
import Data.Binary.Put

-- base

import Control.Monad
import Data.Bits
import Data.List as List
import Data.String

-- containers
import Data.Set as Set

-- nfdata
import Control.DeepSeq (NFData)

-- text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

-- bytestring
import qualified Data.ByteString as BS

-- import           Debug.Trace

{- $SizedDataStructures
 These data structures enables binary reading and writing of lists and
 byte strings that are prepended with the number of elements to read or write.
-}

{- | SizedList is a binary type, that reads a list of elements. It first reads a
 length N of type 'w' and then N items of type 'a'.
-}
newtype SizedList w a = SizedList
  { unSizedList :: [a]
  }
  deriving (Show, Eq, Functor, NFData, Ord)

-- | Get the size of the sized list.
listSize :: Num w => SizedList w a -> w
listSize =
  fromIntegral . length . unSizedList

instance Foldable (SizedList w) where
  foldMap am =
    foldMap am . unSizedList

instance Traversable (SizedList w) where
  traverse afb ta =
    SizedList <$> traverse afb (unSizedList ta)

instance (Binary w, Integral w, Binary a) => Binary (SizedList w a) where
  get = do
    len <- get :: Get w
    Get.label ("SizedList[" ++ show (fromIntegral len :: Int) ++ "]") $
      SizedList <$> replicateM (fromIntegral len) get
  {-# INLINE get #-}

  put sl@(SizedList l) = do
    put (listSize sl)
    forM_ l put
  {-# INLINE put #-}

-- | A byte string with a size w.
newtype SizedByteString w = SizedByteString
  { unSizedByteString :: BS.ByteString
  }
  deriving (Show, Eq, NFData, Ord, IsString)

-- | Get the size of a SizedByteString
byteStringSize :: (Num w) => SizedByteString w -> w
byteStringSize =
  fromIntegral . BS.length . unSizedByteString

instance (Binary w, Integral w) => Binary (SizedByteString w) where
  get = do
    x <- get :: Get w
    SizedByteString <$> getByteString (fromIntegral x)
  put sbs@(SizedByteString bs) = do
    put (byteStringSize sbs)
    putByteString bs

replaceJavaZeroWithNormalZero :: BS.ByteString -> BS.ByteString
replaceJavaZeroWithNormalZero = go
 where
  go bs =
    case BS.breakSubstring "\192\128" bs of
      (h, "") -> h
      (h, t) -> h `BS.append` "\0" `BS.append` go (BS.drop 2 t)

replaceNormalZeroWithJavaZero :: BS.ByteString -> BS.ByteString
replaceNormalZeroWithJavaZero = go
 where
  go bs =
    case BS.breakSubstring "\0" bs of
      (h, "") -> h
      (h, t) -> h `BS.append` "\192\128" `BS.append` go (BS.drop 1 t)

-- | Convert a Sized bytestring to Utf8 Text.
sizedByteStringToText
  :: SizedByteString w
  -> Either TE.UnicodeException Text.Text
sizedByteStringToText (SizedByteString bs) =
  let rst = TE.decodeUtf8' bs
   in case rst of
        Right txt -> Right txt
        Left _ -> tryDecode bs

tryDecode :: BS.ByteString -> Either TE.UnicodeException Text.Text
tryDecode = TE.decodeUtf8' . replaceJavaZeroWithNormalZero

-- | Convert a Sized bytestring from Utf8 Text.
sizedByteStringFromText
  :: Text.Text
  -> SizedByteString w
sizedByteStringFromText t =
  SizedByteString . replaceNormalZeroWithJavaZero . TE.encodeUtf8 $ t

{- $BitSet
 A bit set is a set where each element is represented a bit in a word. This
 section also defines the 'Enumish' type class. It is different than a 'Enum'
 in that the integers they represent does not have to be subsequent.
-}

{- | An Enumish value, all maps to a number, but not all integers maps to a
 enumsish value. There is no guarantee that the integers will be subsequent.
-}
class (Eq a, Ord a) => Enumish a where
  -- | The only needed implementation is a list of integer-enum pairs in
  -- ascending order, corresponding to their integer value.
  inOrder :: [(Int, a)]

  fromEnumish :: a -> Int
  fromEnumish a = case List.find ((== a) . snd) $ inOrder of
    Just (i, _) -> i
    Nothing -> error "Bad implemetnation of Enumish or Eq"

  toEnumish :: Int -> Maybe a
  toEnumish i = snd <$> (List.find ((== i) . fst) $ inOrder)

-- | A bit set of size w
newtype BitSet w a = BitSet
  { toSet :: Set.Set a
  }
  deriving (Ord, Show, Eq, NFData)

bitSetToWord :: (Enumish a, Bits w) => BitSet w a -> w
bitSetToWord =
  toWord . Set.toList . toSet

toWord :: (Enumish a, Bits w) => [a] -> w
toWord =
  List.foldl' (\a -> setBit a . fromEnumish) zeroBits

instance (Show w, Bits w, Binary w, Enumish a) => Binary (BitSet w a) where
  get = do
    word <- get :: Get w
    return . BitSet $ Set.fromList [x | (i, x) <- inOrder, testBit word i]

  put = put . bitSetToWord

-- | A sized list using a 8 bit word as length
type SizedList8 = SizedList Word8

-- | A sized list using a 16 bit word as length
type SizedList16 = SizedList Word16

-- | A sized bytestring using a 32 bit word as length
type SizedByteString32 = SizedByteString Word32

-- | A sized bytestring using a 16 bit word as length
type SizedByteString16 = SizedByteString Word16

-- | A BitSet using a 16 bit word
type BitSet16 = BitSet Word16

-- \$utils
--

-- | Takes the third element of a triple.
trd :: (a, b, c) -> c
trd (_, _, c) = c
