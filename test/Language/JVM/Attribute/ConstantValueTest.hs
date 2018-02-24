{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.ConstantValueTest where

import SpecHelper

import Language.JVM.Attribute.ConstantValue
import Language.JVM.Constant
import Language.JVM.ConstantTest ()

prop_ConstantValue_roundtrip :: ConstantValue High -> Property
prop_ConstantValue_roundtrip = isoRoundtrip

instance Arbitrary (ConstantValue High) where
  arbitrary = genericArbitraryU
