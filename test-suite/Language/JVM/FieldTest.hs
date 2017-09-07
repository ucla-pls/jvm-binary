{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.FieldTest where

import SpecHelper

import Language.JVM.Field
import Language.JVM.UtilsTest ()
import Language.JVM.ConstantTest ()
import Language.JVM.AttributeTest ()

prop_encode_and_decode :: Field -> Property
prop_encode_and_decode = isoBinary

instance Arbitrary Field where
  arbitrary = Field
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary AccessFlags where
  arbitrary = AccessFlags <$> arbitrary

instance Arbitrary AccessFlag where
  arbitrary = toEnum <$> choose (0, 15)
