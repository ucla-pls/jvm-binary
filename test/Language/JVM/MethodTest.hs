{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
module Language.JVM.MethodTest where

import           SpecHelper

import           Language.JVM.Attribute.CodeTest       ()
import           Language.JVM.Attribute.ExceptionsTest ()
import           Language.JVM.AttributeTest            ()
import           Language.JVM.ConstantTest             ()
import           Language.JVM.UtilsTest                ()

import           Language.JVM

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
