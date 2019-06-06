{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
module Language.JVM.MethodSpec where

import           SpecHelper

import           Language.JVM.Attribute.CodeSpec       ()
import           Language.JVM.Attribute.ExceptionsSpec ()
import           Language.JVM.AttributeSpec            ()
import           Language.JVM.ConstantSpec             ()
import           Language.JVM.UtilsSpec                ()

import           Language.JVM

spec :: Spec
spec =
  it "can do a roundtrip" $ property $ prop_roundtrip_Method

prop_roundtrip_Method :: Method High -> Property
prop_roundtrip_Method = isoRoundtrip

instance Arbitrary (MethodAttributes High) where
  arbitrary =
    MethodAttributes <$> pure [] <*> arbitrary <*> pure [] <*> pure []
  shrink (MethodAttributes a b c d )  =
    MethodAttributes <$> shrink a <*> shrink b <*> pure c <*> pure d

instance Arbitrary (Method High) where
  arbitrary =
    Method <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Method a b c d) =
    Method <$> shrink a <*> shrink b <*> shrink c <*> shrink d
