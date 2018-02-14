{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.ClassFileTest where

import SpecHelper

import qualified Data.IntMap as IM

import Language.JVM.ClassFile
import Language.JVM.Constant

import Language.JVM.Utils
import Language.JVM.UtilsTest ()
import Language.JVM.AttributeTest ()
import Language.JVM.ConstantTest ()
import Language.JVM.FieldTest ()
import Language.JVM.MethodTest ()

prop_encode_and_decode :: ClassFile Index -> Property
prop_encode_and_decode = isoBinary

instance Arbitrary (ClassFile Index) where
  arbitrary = ClassFile
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (pure $ ConstantPool IM.empty)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (pure $ SizedList [])
    <*> (pure $ SizedList [])
    <*> (pure $ SizedList [])
    <*> (pure $ SizedList [])
