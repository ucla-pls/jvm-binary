{-# OPTIONS_GHC -fno-warn-orphans #-}
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

spec_reading_classfile :: Spec
spec_reading_classfile = do
  beforeAll (blReadFile "test-suite/project/Main.class") $ do
    it "can read the bytestring" $ \bs ->
      let classfile = decode bs
      in (magicNumber classfile) `shouldBe` 3405691582

prop_encode_and_decode :: ClassFile -> Property
prop_encode_and_decode = isoBinary

instance Arbitrary ClassFile where
  arbitrary = ClassFile
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (pure $ ConstantPool IM.empty)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (pure $ SizedList16 [])
    <*> (pure $ SizedList16 [])
    <*> (pure $ SizedList16 [])
    <*> (pure $ SizedList16 [])

instance Arbitrary AccessFlags where
  arbitrary = AccessFlags <$> arbitrary

instance Arbitrary AccessFlag where
  arbitrary = toEnum <$> choose (0, 15)
