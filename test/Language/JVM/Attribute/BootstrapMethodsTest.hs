{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.BootstrapMethodsTest where

import SpecHelper

import Language.JVM.Attribute.BootstrapMethods
import Language.JVM.Constant
import Language.JVM.ConstantTest ()

prop_BootstrapMethods_roundtrip :: BootstrapMethods High -> Property
prop_BootstrapMethods_roundtrip = isoRoundtrip

instance Arbitrary (BootstrapMethods High) where
  arbitrary = genericArbitraryU

instance Arbitrary (BootstrapMethod High) where
  arbitrary = genericArbitraryU
