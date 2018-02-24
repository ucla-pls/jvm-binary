{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.FieldTest where

import SpecHelper

import Language.JVM.Field
import Language.JVM.UtilsTest ()
import Language.JVM.ConstantTest ()
import Language.JVM.AttributeTest ()
import Language.JVM.Attribute.ConstantValueTest ()
import Language.JVM.Constant

prop_roundtrip :: Field High -> Property
prop_roundtrip = isoRoundtrip

instance Arbitrary (FieldAttributes High) where
  arbitrary =
    FieldAttributes <$> arbitrary <*> pure []

instance Arbitrary (Field High) where
  arbitrary = Field
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
