{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.MethodTest where

import SpecHelper

import Language.JVM.Method
import Language.JVM.UtilsTest ()
import Language.JVM.ConstantTest ()
import Language.JVM.AttributeTest ()
import Language.JVM.Constant

prop_encode_and_decode :: Method Index -> Property
prop_encode_and_decode = isoBinary

instance Arbitrary (Method Index) where
  arbitrary = Method
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
