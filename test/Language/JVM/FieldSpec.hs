{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.FieldSpec where

import SpecHelper

import Language.JVM.UtilsSpec ()
import Language.JVM.ConstantSpec ()
import Language.JVM.AttributeSpec ()
import Language.JVM.Attribute.ConstantValueSpec ()

import Language.JVM

spec :: Spec
spec =
  it "can do a roundtrip" $ property $ prop_roundtrip_Field

prop_roundtrip_Field :: Field High -> Property
prop_roundtrip_Field = isoRoundtrip

instance Arbitrary (FieldAttributes High) where
  arbitrary =
    FieldAttributes <$> arbitrary <*> pure [] <*> pure []

instance Arbitrary (Field High) where
  arbitrary = Field
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
