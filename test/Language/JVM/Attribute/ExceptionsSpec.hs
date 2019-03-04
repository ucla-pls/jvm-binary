{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.ExceptionsSpec where

import           SpecHelper

import           Language.JVM

import           Language.JVM.ConstantSpec         ()

spec :: Spec
spec =
  it "can do a roundtrip" $ property $ prop_roundtrip_Exceptions

prop_roundtrip_Exceptions :: Exceptions High -> Property
prop_roundtrip_Exceptions = isoRoundtrip

instance Arbitrary (Exceptions High) where
  arbitrary = genericArbitraryU
