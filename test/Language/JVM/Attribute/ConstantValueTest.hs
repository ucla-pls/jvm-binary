{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.ConstantValueTest where

import SpecHelper

import Language.JVM.ConstantTest ()

import Language.JVM

prop_roundtrip_ConstantValue :: ConstantValue High -> Property
prop_roundtrip_ConstantValue = isoRoundtrip

instance Arbitrary (ConstantValue High) where
  arbitrary = genericArbitraryU
