{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.Attribute.CodeTest where

-- -- import           Data.Int
import qualified Data.Vector as V
import           Generic.Random
import           SpecHelper

import           Language.JVM.AttributeTest  ()
import           Language.JVM.Attribute.Code
import           Language.JVM.Constant       (Index)
import           Language.JVM.UtilsTest      ()


-- prop_encode_and_decode_ByteCode :: ByteCode -> Property
-- prop_encode_and_decode_ByteCode = isoBinary

-- prop_encode_and_decode :: Code Attribute -> Property
-- prop_encode_and_decode = isoBinary

prop_encode_and_decode_ByteCodeOpr :: ByteCodeOpr Index -> Property
prop_encode_and_decode_ByteCodeOpr = isoBinary

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
  arbitrary =
   genericArbitrary uniform

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance Arbitrary a => Arbitrary (ArrayType a) where
  arbitrary =
    genericArbitrary uniform

instance Arbitrary LocalAddress where
  arbitrary =
    genericArbitrary uniform

instance Arbitrary BinOpr where
  arbitrary = genericArbitrary uniform

instance Arbitrary BitOpr where
  arbitrary = genericArbitrary uniform

instance Arbitrary OneOrTwo where
  arbitrary = genericArbitrary uniform

instance Arbitrary IncrementAmount where
  arbitrary = genericArbitrary uniform

instance Arbitrary SmallArithmeticType where
  arbitrary = genericArbitrary uniform

instance Arbitrary CmpOpr where
  arbitrary = genericArbitrary uniform

instance Arbitrary FieldAccess where
  arbitrary = genericArbitrary uniform

instance Arbitrary Invokation where
  arbitrary = genericArbitrary uniform

instance Arbitrary (CConstant Index) where
  arbitrary = genericArbitrary uniform

instance Arbitrary (ByteCode Index) where
  arbitrary = ByteCode <$> arbitrary

instance Arbitrary (ExceptionTable Index) where
  arbitrary = ExceptionTable
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
