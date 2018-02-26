{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.ExceptionsTest where

import           SpecHelper

import           Language.JVM

import           Language.JVM.ConstantTest         ()

prop_roundtrip_Exceptions :: Exceptions High -> Property
prop_roundtrip_Exceptions = isoRoundtrip

instance Arbitrary (Exceptions High) where
  arbitrary = genericArbitraryU
