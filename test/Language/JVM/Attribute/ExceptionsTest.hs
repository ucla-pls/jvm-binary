{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.ExceptionsTest where

import SpecHelper

import Language.JVM.Attribute.Exceptions
import Language.JVM.Constant
import Language.JVM.ConstantTest ()

prop_Exceptions_roundtrip :: Exceptions High -> Property
prop_Exceptions_roundtrip = isoRoundtrip

instance Arbitrary (Exceptions High) where
  arbitrary = genericArbitraryU
