{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JVM.Attribute.ConstantValueSpec where

import SpecHelper

import Language.JVM.ConstantSpec ()

import Language.JVM
import Language.JVM.Attribute.ConstantValue

spec :: Spec
spec = do
  it "can do a roundtrip on a VClass" $
    prop_roundtrip_ConstantValue (ConstantValue (VClass "Lthis/class;"))
  it "can do a roundtrip" $ property $ prop_roundtrip_ConstantValue

prop_roundtrip_ConstantValue :: ConstantValue High -> Property
prop_roundtrip_ConstantValue = isoRoundtrip

instance Arbitrary (ConstantValue High) where
  arbitrary = genericArbitraryU
