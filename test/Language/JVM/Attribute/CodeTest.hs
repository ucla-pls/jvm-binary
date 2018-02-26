{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.Attribute.CodeTest where

import qualified Data.Vector                                as V
import           Generic.Random

import           SpecHelper

import           Language.JVM.Attribute.LineNumberTableTest ()
import           Language.JVM.Attribute.StackMapTableTest   ()
import           Language.JVM.AttributeTest                 ()
import           Language.JVM.UtilsTest                     ()

import           Language.JVM
import           Language.JVM.Attribute.Code

prop_roundtrip_Code :: Code High -> Property
prop_roundtrip_Code = isoRoundtrip

prop_roundtrip_ByteCode :: ByteCode High -> Property
prop_roundtrip_ByteCode = isoRoundtrip

prop_roundtrip_ExceptionTable :: ExceptionTable High -> Property
prop_roundtrip_ExceptionTable = isoRoundtrip

prop_roundtrip_ByteCodeOpr :: ByteCodeOpr High -> Property
prop_roundtrip_ByteCodeOpr = isoRoundtrip

instance Arbitrary (Code High) where
  arbitrary = Code
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink (Code ms ml bc ac xs) =
    Code ms ml <$> shrink bc <*> shrink ac <*> shrink xs

instance Arbitrary (CodeAttributes High) where
  arbitrary = genericArbitraryU

instance Arbitrary (ByteCode High) where
  arbitrary =
    ByteCode <$> arbitrary

  shrink (ByteCode xs) =
    ByteCode <$> shrink xs

instance Arbitrary ArithmeticType where
  arbitrary = elements [ MInt, MLong, MFloat, MDouble ]

instance Arbitrary LocalType where
  arbitrary = elements [ LInt, LLong, LFloat, LDouble, LRef ]

instance Arbitrary (ByteCodeOpr High) where
  arbitrary =
    genericArbitraryU

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance Arbitrary ArrayType where
  arbitrary = genericArbitrary uniform

instance Arbitrary (ExactArrayType High) where
  arbitrary =
    oneof
    [ pure EABoolean
    , pure EAByte
    , pure EAChar
    , pure EAShort
    , pure EAInt
    , pure EALong
    , pure EAFloat
    , pure EADouble
    , EARef <$> arbitrary
    ]

instance Arbitrary BinOpr where
  arbitrary = genericArbitrary uniform

instance Arbitrary CastOpr where
  arbitrary =
    oneof . map pure $
      [ CastTo MInt MLong
      , CastTo MInt MFloat
      , CastTo MInt MDouble

      , CastTo MLong MInt
      , CastTo MLong MFloat
      , CastTo MLong MDouble

      , CastTo MFloat MInt
      , CastTo MFloat MLong
      , CastTo MFloat MDouble

      , CastTo MDouble MInt
      , CastTo MDouble MLong
      , CastTo MDouble MFloat

      , CastDown MByte
      , CastDown MChar
      , CastDown MShort
      ]

instance Arbitrary BitOpr where
  arbitrary = genericArbitraryU

instance Arbitrary OneOrTwo where
  arbitrary = genericArbitraryU

instance Arbitrary SmallArithmeticType where
  arbitrary = genericArbitraryU

instance Arbitrary CmpOpr where
  arbitrary = genericArbitraryU

instance Arbitrary FieldAccess where
  arbitrary = genericArbitrary uniform

instance Arbitrary (Invokation High) where
  arbitrary =
    oneof
    [ InvkSpecial <$> arbitrary
    , InvkVirtual <$> arbitrary
    , InvkStatic <$> arbitrary
    , InvkInterface <$> (arbitrary `suchThat` \i -> i > 0) <*> arbitrary
    , InvkDynamic <$> arbitrary
    ]
instance Arbitrary (CConstant High) where
  arbitrary = genericArbitraryU

instance Arbitrary SwitchTable where
  arbitrary = genericArbitraryU

instance Arbitrary (ExceptionTable High) where
  arbitrary = ExceptionTable
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
