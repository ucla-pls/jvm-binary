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
