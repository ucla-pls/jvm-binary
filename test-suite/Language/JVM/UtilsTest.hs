{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.UtilsTest where

import SpecHelper

import Data.Set as Set
import Language.JVM.Utils

instance Arbitrary a => Arbitrary (SizedList16 a) where
  arbitrary =
    SizedList16 <$> arbitrary

-- instance Arbitrary a => Arbitrary (SizedList32 a) where
--   arbitrary =
--     SizedList32 <$> arbitrary

instance (Enumish a) => Arbitrary (BitSet16 a) where
  arbitrary =
    BitSet16 . Set.fromList <$> sublistOf (snd <$> inOrder)
