{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.FieldTest where

import SpecHelper

import Language.JVM.Field
import Language.JVM.UtilsTest ()
import Language.JVM.ConstantTest ()
import Language.JVM.AttributeTest ()
import Language.JVM.Constant

prop_encode_and_decode :: Field Index -> Property
prop_encode_and_decode = isoBinary

instance Arbitrary (Field Index) where
  arbitrary = Field
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
