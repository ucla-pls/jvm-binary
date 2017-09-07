{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.UtilsTest where

import SpecHelper

import Language.JVM.Utils

instance Arbitrary a => Arbitrary (SizedList16 a) where
  arbitrary =
    SizedList16 <$> arbitrary

-- instance Arbitrary a => Arbitrary (SizedList32 a) where
--   arbitrary =
--     SizedList32 <$> arbitrary
