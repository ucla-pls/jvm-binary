{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.Attribute.CodeTest where

import           SpecHelper

-- -- -- import           Data.Word

-- -- -- -- import           Language.JVM.Attribute     (Attribute)
import           Language.JVM.AttributeTest ()

import           Language.JVM.Attribute.Code
import           Language.JVM.Constant  (Index)
import           Language.JVM.UtilsTest ()


-- prop_encode_and_decode_ByteCode :: ByteCode -> Property
-- prop_encode_and_decode_ByteCode = isoBinary

-- prop_encode_and_decode :: Code Attribute -> Property
-- prop_encode_and_decode = isoBinary

instance Arbitrary (Code Index) where
  arbitrary = Code
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


instance Arbitrary ArithmeticType where
  arbitrary = elements [ MInt, MLong, MFloat, MDouble ]

instance Arbitrary LocalType where
  arbitrary = elements [ LInt, LLong, LFloat, LDouble, LRef ]

instance Arbitrary (ByteCodeInst Index) where
  arbitrary = ByteCodeInst <$> arbitrary <*> arbitrary

instance Arbitrary (ByteCodeOpr Index) where
  arbitrary = oneof
    [ pure Nop
    , Push <$> arbitrary
    ]

instance Arbitrary (CConstant Index) where
  arbitrary = oneof
    [ pure CNull
    , pure CIntM1
    , pure CInt0
    , pure CInt1
    , pure CInt2
    , pure CInt3
    , pure CInt4
    , pure CInt5

    , pure CLong0
    , pure CLong1

    , pure CFloat0
    , pure CFloat1
    , pure CFloat2

    , pure CDouble0
    , pure CDouble1

    , CByte <$> arbitrary
    , CShort <$> arbitrary

    , CHalfRef <$> arbitrary
    , CRef One <$> arbitrary
    , CRef Two <$> arbitrary
    ]

instance Arbitrary (ByteCode Index) where
  arbitrary = ByteCode <$> arbitrary

instance Arbitrary (ExceptionTable Index) where
  arbitrary = ExceptionTable
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary