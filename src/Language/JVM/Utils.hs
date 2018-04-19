{-|
Module      : Language.JVM.Utils
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains utilities missing not in other libraries.
-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Language.JVM.Utils
  ( -- * Sized Data Structures
    --
    -- $SizedDataStructures
    SizedList (..)
  , listSize

  , SizedByteString (..)
  , byteStringSize

    -- ** Specific sizes
  , SizedList16
  , SizedByteString32
  , SizedByteString16
  , sizedByteStringFromText
  , sizedByteStringToText

    -- * Bit Set
    --
    -- $BitSet
  , BitSet (..)
  , Enumish(..)

    -- ** Specific sizes
  , BitSet16

  -- * General Utilities
  -- $utils
  , trd
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.List                as List
import           Data.Set                 as Set
import           Data.String

import           Control.DeepSeq          (NFData)
import           Control.Monad

import qualified Data.Text                as Text

import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE

import qualified Data.ByteString          as BS

import           Debug.Trace


-- $SizedDataStructures
-- These data structures enables binary reading and writing of lists and
-- byte strings that are prepended with the number of elements to read or write.


-- | SizedList is a binary type, that reads a list of elements. It first reads a
-- length N of type 'w' and then N items of type 'a'.
newtype SizedList w a = SizedList
  { unSizedList :: [ a ]
  } deriving (Show, Eq, Functor, NFData, Ord)

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
    SizedList <$> replicateM (fromIntegral len) get

  put sl@(SizedList l) = do
    put (listSize sl)
    forM_ l put

-- | A byte string with a size w.
newtype SizedByteString w = SizedByteString
  { unSizedByteString :: BS.ByteString
  } deriving (Show, Eq, NFData, Ord, IsString)

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

-- | Convert a Sized bytestring to Utf8 Text.
sizedByteStringToText ::
     SizedByteString w
  -> Either TE.UnicodeException Text.Text
sizedByteStringToText bs =
  Right . TE.decodeUtf8With (\msg x -> traceShow x Nothing) . unSizedByteString $ bs
  -- case TE.decodeUtf8With (const error) . unSizedByteString $ bs of
  --   Left a
  --    | bs == "\192\128" ->
  --      Right (Text.pack ['\0'])
  --    | otherwise -> Left a
  --   Right x ->
  --     Right x

-- | Convert a Sized bytestring from Utf8 Text.
sizedByteStringFromText ::
     Text.Text
  -> SizedByteString w
sizedByteStringFromText t
  | t == "\0" =
    SizedByteString "\192\128"
  | otherwise =
    SizedByteString . TE.encodeUtf8 $ t

-- $BitSet
-- A bit set is a set where each element is represented a bit in a word. This
-- section also defines the 'Enumish' type class. It is different than a 'Enum'
-- in that the integers they represent does not have to be subsequent.

-- | An Enumish value, all maps to a number, but not all integers maps to a
-- enumsish value. There is no guarantee that the integers will be subsequent.
class (Eq a, Ord a) => Enumish a where
  -- | The only needed implementation is a list of integer-enum pairs in
  -- ascending order, corresponding to their integer value.
  inOrder :: [(Int, a)]

  fromEnumish :: a -> Int
  fromEnumish a = let Just (i, _) = List.find ((== a) . snd) $ inOrder in i

  toEnumish :: Int -> Maybe a
  toEnumish i = snd <$> (List.find ((== i) . fst) $ inOrder)

-- | A bit set of size w
newtype BitSet w a = BitSet
  { toSet :: Set.Set a
  } deriving (Ord, Show, Eq, NFData)

instance (Bits w, Binary w, Enumish a) => Binary (BitSet w a) where
  get = do
    word <- get :: Get w
    return . BitSet $ Set.fromList [ x | (i, x) <- inOrder, testBit word i ]

  put (BitSet f) = do
    let word =
          List.foldl' setBit zeroBits
            (List.map fromEnumish $ Set.toList f) :: w
    put word

-- | A sized list using a 16 bit word as length
type SizedList16 = SizedList Word16

-- | A sized bytestring using a 32 bit word as length
type SizedByteString32 = SizedByteString Word32

-- | A sized bytestring using a 16 bit word as length
type SizedByteString16 = SizedByteString Word16

-- | A BitSet using a 16 bit word
type BitSet16 = BitSet Word16

{- $utils

-}

-- | Takes the third element of a triple.
trd :: (a, b, c) -> c
trd (_, _, c) = c
