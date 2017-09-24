{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.UtilsTest where

import SpecHelper

import qualified Data.ByteString as BS

import Data.Set as Set
import Language.JVM.Utils

instance Arbitrary a => Arbitrary (SizedList w a) where
  arbitrary =
    SizedList <$> arbitrary

instance Arbitrary (SizedByteString w) where
  arbitrary = do
    len <- choose (0, 50)
    SizedByteString . BS.pack <$> sequence (replicate len arbitrary)

instance (Enumish a) => Arbitrary (BitSet w a) where
  arbitrary =
    BitSet . Set.fromList <$> sublistOf (snd <$> inOrder)
