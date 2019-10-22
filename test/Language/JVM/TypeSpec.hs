{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.JVM.TypeSpec where

import SpecHelper

import Data.Either
import Language.JVM.Type

spec :: Spec
spec = do
  describe "JType parsing" $ do
    it "can parse \"[B\" as an array" $
      deserialize "[B" `shouldBe` Right (JTRef (JTArray (JTBase JTByte)))
    it "can parse an array of strings" $
      deserialize "[Ljava/lang/String;" `shouldBe`
        Right (JTRef (JTArray (JTRef (JTClass "java/lang/String"))))

  describe "MethodDescriptor parsing" $ do
    it "can parse the empty method" $
      deserialize "()V" `shouldBe`
        Right (MethodDescriptor [] "V")
    it "can parse method arguments" $
      deserialize "(BZ)B" `shouldBe`
        Right (MethodDescriptor [JTBase JTByte, JTBase JTBoolean] "B")
    it "does not parse if there is too much" $
      (deserialize "(BZ)Bx" :: Either String MethodDescriptor) `shouldSatisfy` isLeft

  describe "serializes" $ do
    it "JType" . property $ \(t :: JType) ->
      deserialize (serialize t) === Right t
    it "MethodDescriptor" . property $ \(t :: MethodDescriptor) ->
      deserialize (serialize t) === Right t
    it "FieldDescriptor" . property $ \(t :: FieldDescriptor) ->
      deserialize (serialize t) === Right t
    it "Flat JRefType" . property $ \(t :: JRefType) ->
      deserializeWith parseFlatJRefType (serializeWith serializeFlatJRefType t) === Right t
    it "AbsMethodId" . property $ \(t :: AbsMethodId) ->
      deserialize (serialize t) === Right t
    it "AbsFieldId" . property $ \(t :: AbsFieldId) ->
      deserialize (serialize t) === Right t

instance Arbitrary ClassName where
  arbitrary = pure $ "package/Main"

instance Arbitrary JType where
  arbitrary = genericArbitrary uniform

instance Arbitrary JBaseType where
  arbitrary = genericArbitrary uniform

instance Arbitrary JRefType where
  arbitrary = genericArbitrary uniform

instance Arbitrary ReturnDescriptor where
  arbitrary = genericArbitrary uniform

instance Arbitrary MethodDescriptor where
  arbitrary = genericArbitrary uniform

instance Arbitrary FieldDescriptor where
  arbitrary = genericArbitrary uniform

instance Arbitrary FieldId where
  arbitrary = FieldId <$> arbitrary

instance Arbitrary MethodId where
  arbitrary = MethodId <$> arbitrary

instance Arbitrary a => Arbitrary (InClass a) where
  arbitrary = InClass <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (InRefType a) where
  arbitrary = InRefType <$> arbitrary <*> arbitrary

instance Arbitrary AbsFieldId where
  arbitrary = AbsFieldId <$> arbitrary

instance Arbitrary AbsMethodId where
  arbitrary = AbsMethodId <$> arbitrary


instance Arbitrary t => Arbitrary (NameAndType t) where
  arbitrary =
    NameAndType
    <$> elements ["a", "f", "x", "y"]
    <*> arbitrary
