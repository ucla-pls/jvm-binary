{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.ConstantValueSpec where

import SpecHelper

import Language.JVM.ConstantSpec ()

import Language.JVM

spec :: Spec
spec =
  it "can do a roundtrip" $ property $ prop_roundtrip_ConstantValue

prop_roundtrip_ConstantValue :: ConstantValue High -> Property
prop_roundtrip_ConstantValue = isoRoundtrip

instance Arbitrary (ConstantValue High) where
  arbitrary = genericArbitraryU
