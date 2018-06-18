{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.JVM.TypeTest where

import SpecHelper

import Language.JVM.Type

-- import Text.Megaparsec
-- import Test.Hspec.Megaparsec

-- spec_JType_parsing :: Spec
-- spec_JType_parsing = do
--   it "can parse \"[B\" as an array" $
--     parse parseJType "" "[B" `shouldParse` JTArray JTByte
--   it "can parse an array of strings" $
--     parse parseJType "" "[Ljava/lang/String;" `shouldParse`
--       JTArray (JTClass (ClassName "java/lang/String"))

-- spec_MethodDescriptor_parsing :: Spec
-- spec_MethodDescriptor_parsing = do
--   it "can parse the empty method" $
--     parse parseMethodDescriptor "" "()V" `shouldParse`
--       MethodDescriptor [] Nothing
--   it "can parse method arguments" $
--     parse parseMethodDescriptor "" "(BZ)B" `shouldParse`
--       MethodDescriptor [JTByte, JTBoolean] (Just JTByte)
--   it "does not parse if there is too much" $
--     methodDescriptorFromText "(BZ)Bx" `shouldBe` Nothing

instance Arbitrary ClassName where
  arbitrary = pure $ ClassName "package.Main"

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
