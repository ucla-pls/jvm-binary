{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.ClassFileTest where

import SpecHelper

-- import qualified Data.IntMap as IM

import Language.JVM.ClassFile
-- import Language.JVM.ConstantPool
import Language.JVM.Constant

import Language.JVM.Utils
import Language.JVM.UtilsTest ()
import Language.JVM.AttributeTest ()
import Language.JVM.Attribute.BootstrapMethodsTest ()
import Language.JVM.ConstantTest ()
import Language.JVM.FieldTest ()
import Language.JVM.MethodTest ()

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
    <*> (pure $ SizedList [])
    <*> (pure $ SizedList [])
    <*> arbitrary

  shrink ClassFile{..} = do
    cInterfaceIndicies' <- shrink cInterfaceIndicies'
    cAttributes <- shrink cAttributes
    return $ClassFile {..}
