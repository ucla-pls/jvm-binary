{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.BootstrapMethodsSpec where

import           SpecHelper

import           Language.JVM.ConstantSpec               ()

import           Language.JVM.Attribute.BootstrapMethods
import           Language.JVM

spec :: Spec
spec =
  it "can do a roundtrip" $ property $ prop_roundtrip_BootstrapMethods

prop_roundtrip_BootstrapMethods :: BootstrapMethods High -> Property
prop_roundtrip_BootstrapMethods = isoRoundtrip

instance Arbitrary (BootstrapMethods High) where
  arbitrary = genericArbitraryU

instance Arbitrary (BootstrapMethod High) where
  arbitrary =
    BootstrapMethod <$> arbitrary <*> (SizedList <$> listOf (resize 1 arbitrary))
