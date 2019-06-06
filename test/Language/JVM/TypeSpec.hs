{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.JVM.TypeSpec where

import SpecHelper

import Data.Attoparsec.Text
import Data.Either
import Language.JVM.Type

-- import Text.Megaparsec
-- import Spec.Hspec.Megaparsec

spec :: Spec
spec = do
  describe "JType parsing" $ do
    it "can parse \"[B\" as an array" $
      parseOnly parseType "[B" `shouldBe` Right (JTRef (JTArray (JTBase JTByte)))
    it "can parse an array of strings" $
      parseOnly parseType "[Ljava/lang/String;" `shouldBe`
        Right (JTRef (JTArray (JTRef (JTClass (ClassName "java/lang/String")))))

  describe "MethodDescriptor parsing" $ do
    it "can parse the empty method" $
      parseOnly parseType "()V" `shouldBe`
        Right (MethodDescriptor [] Nothing)
    it "can parse method arguments" $
      parseOnly parseType "(BZ)B" `shouldBe`
        Right (MethodDescriptor [JTBase JTByte, JTBase JTBoolean] (Just (JTBase JTByte)))
    it "does not parse if there is too much" $
      (typeFromText "(BZ)Bx" :: Either String MethodDescriptor) `shouldSatisfy` isLeft

instance Arbitrary ClassName where
  arbitrary = pure $ ClassName "package/Main"

instance Arbitrary JType where
  arbitrary = genericArbitrary uniform

instance Arbitrary JBaseType where
  arbitrary = genericArbitrary uniform

instance Arbitrary JRefType where
  arbitrary = genericArbitrary uniform

instance Arbitrary MethodDescriptor where
  arbitrary = genericArbitrary uniform

instance Arbitrary FieldDescriptor where
  arbitrary = genericArbitrary uniform

instance Arbitrary t => Arbitrary (NameAndType t) where
  arbitrary =
    NameAndType
    <$> elements ["a", "f", "x", "y"]
    <*> arbitrary
