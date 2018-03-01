{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.ClassFileTest where

import           SpecHelper

import           Language.JVM

import           Language.JVM.Attribute.BootstrapMethodsTest ()
import           Language.JVM.AttributeTest                  ()
import           Language.JVM.ConstantTest                   ()
import           Language.JVM.FieldTest                      ()
import           Language.JVM.MethodTest                     ()
import           Language.JVM.Utils
import           Language.JVM.UtilsTest                      ()

prop_roundtrip_ClassFile :: ClassFile High -> Property
prop_roundtrip_ClassFile = isoRoundtrip

instance Arbitrary (ClassAttributes High) where
  arbitrary = ClassAttributes <$> pure [] <*> pure []

instance Arbitrary (ClassFile High) where
  arbitrary = ClassFile
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (pure ())
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (resize 2 arbitrary)
    <*> (resize 2 arbitrary)
    <*> arbitrary

  shrink ClassFile{..} = do
    cInterfaceIndicies' <- shrink cInterfaceIndicies'
    cAttributes <- shrink cAttributes
    cMethods' <- shrink cMethods'
    cFields' <- shrink cFields'
    return $ClassFile {..}
