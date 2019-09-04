{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards #-}
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
  prop "can do a roundtrip" prop_roundtrip_Method

prop_roundtrip_Method :: Method High -> Property
prop_roundtrip_Method = isoRoundtrip

instance Arbitrary (MethodAttributes High) where
  arbitrary =
    (\a -> emptyMethodAttributes { maExceptions = a }) <$> arbitrary
  shrink a@MethodAttributes {..} =
    (\e -> a { maExceptions = e }) <$> shrink maExceptions

instance Arbitrary (Method High) where
  arbitrary =
    Method <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Method a b c d) =
    Method <$> shrink a <*> shrink b <*> shrink c <*> shrink d
