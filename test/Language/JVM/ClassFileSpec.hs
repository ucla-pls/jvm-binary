{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JVM.ClassFileSpec where

import SpecHelper

import Language.JVM

import Language.JVM.Attribute.BootstrapMethodsSpec ()
import Language.JVM.AttributeSpec ()
import Language.JVM.ConstantSpec ()
import Language.JVM.FieldSpec ()
import Language.JVM.MethodSpec ()
import Language.JVM.UtilsSpec ()

spec :: Spec
spec =
  prop "can do a roundtrip" prop_roundtrip_ClassFile

prop_roundtrip_ClassFile :: ClassFile High -> Property
prop_roundtrip_ClassFile = isoRoundtrip

instance Arbitrary (ClassAttributes High) where
  arbitrary = pure emptyClassAttributes

instance Arbitrary (ClassFile High) where
  arbitrary =
    ClassFile
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
    cInterfaces <- shrink cInterfaces
    cAttributes <- shrink cAttributes
    cMethods' <- shrink cMethods'
    cFields' <- shrink cFields'
    return $ ClassFile{..}
