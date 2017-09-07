{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.MethodTest where

import SpecHelper

import Language.JVM.Method
import Language.JVM.UtilsTest ()
import Language.JVM.ConstantTest ()
import Language.JVM.AttributeTest ()

prop_encode_and_decode :: Method -> Property
prop_encode_and_decode = isoBinary

instance Arbitrary Method where
  arbitrary = Method
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary AccessFlags where
  arbitrary = AccessFlags <$> arbitrary

instance Arbitrary AccessFlag where
  arbitrary = toEnum <$> choose (0, 15)
