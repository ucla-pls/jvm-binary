{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.JVM.TypeTest where

import SpecHelper

import Data.Attoparsec.Text
import Data.Either
import Language.JVM.Type

-- import Text.Megaparsec
-- import Test.Hspec.Megaparsec

spec_JType_parsing :: Spec
spec_JType_parsing = do
  it "can parse \"[B\" as an array" $
    parseOnly parseText "[B" `shouldBe` Right (JTArray (JTBase JTByte))
  it "can parse an array of strings" $
    parseOnly parseText "[Ljava/lang/String;" `shouldBe`
      Right (JTArray (JTClass (ClassName "java/lang/String")))

spec_MethodDescriptor_parsing :: Spec
spec_MethodDescriptor_parsing = do
  it "can parse the empty method" $
    parseOnly parseText "()V" `shouldBe`
      Right (MethodDescriptor [] Nothing)
  it "can parse method arguments" $
    parseOnly parseText "(BZ)B" `shouldBe`
      Right (MethodDescriptor [JTBase JTByte, JTBase JTBoolean] (Just (JTBase JTByte)))
  it "does not parse if there is too much" $
    (fromText "(BZ)Bx" :: Either String MethodDescriptor) `shouldSatisfy` isLeft

instance Arbitrary ClassName where
  arbitrary = pure $ ClassName "package/Main"

instance Arbitrary JType where
  arbitrary = genericArbitrary uniform

instance Arbitrary JBaseType where
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
