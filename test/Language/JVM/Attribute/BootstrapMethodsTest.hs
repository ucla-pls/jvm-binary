{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.BootstrapMethodsTest where

import SpecHelper

import Language.JVM.Attribute.BootstrapMethods
import Language.JVM.Constant
import Language.JVM.ConstantTest ()

prop_roundtrip_BootstrapMethods :: BootstrapMethods High -> Property
prop_roundtrip_BootstrapMethods = isoRoundtrip

instance Arbitrary (BootstrapMethods High) where
  arbitrary = genericArbitraryU

instance Arbitrary (BootstrapMethod High) where
  arbitrary = genericArbitraryU
