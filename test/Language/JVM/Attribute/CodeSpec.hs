{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JVM.Attribute.CodeSpec where

import qualified Data.Vector as V
import Generic.Random

import SpecHelper

import Language.JVM.Attribute.LineNumberTableSpec ()
import Language.JVM.Attribute.StackMapTableSpec ()
import Language.JVM.AttributeSpec ()
import Language.JVM.UtilsSpec ()

import Language.JVM
import Language.JVM.Attribute.Code

spec :: Spec
spec = do
  prop "can do a roundtrip on Code" prop_roundtrip_Code
  prop "can do a roundtrip on ExceptionTable" prop_roundtrip_ExceptionTable
  prop "can do a roundtrip on ByteCodeInst" prop_roundtrip_ByteCodeInst
  spec_ByteCode_examples

prop_roundtrip_Code :: Code High -> Property
prop_roundtrip_Code = isoRoundtrip

-- prop_roundtrip_ByteCode :: ByteCode High -> Property
-- prop_roundtrip_ByteCode = isoRoundtrip

prop_roundtrip_ExceptionTable :: ExceptionTable High -> Property
prop_roundtrip_ExceptionTable = isoByteCodeRoundtrip

prop_roundtrip_ByteCodeInst :: ByteCodeInst High -> Property
prop_roundtrip_ByteCodeInst = isoByteCodeRoundtrip

spec_ByteCode_examples :: SpecWith ()
spec_ByteCode_examples = do
  it "can round trip 'push (i127)'" $ do
    let opr = ByteCodeInst 0 (Push (Just (VInteger 127)))
    (snd <$> byteCodeRoundtrip opr) `shouldBe` Right opr

  it "can round trip 'push (i-129)'" $ do
    let opr = ByteCodeInst 0 (Push (Just (VInteger (-129))))
    (snd <$> byteCodeRoundtrip opr) `shouldBe` Right opr

instance Arbitrary (Code High) where
  arbitrary = do
    bc <- arbitrary
    Code
      <$> arbitrary
      <*> arbitrary
      <*> pure bc
      <*> (SizedList <$> listOf (genExceptionTable (fromIntegral . V.length . byteCodeInstructions $ bc)))
      <*> pure emptyCodeAttributes

genExceptionTable :: Int -> Gen (ExceptionTable High)
genExceptionTable i =
  ExceptionTable
    <$> arbitraryRef i
    <*> arbitraryRef i
    <*> arbitraryRef i
    <*> arbitrary

arbitraryRef :: Int -> Gen Int
arbitraryRef i =
  choose (0, i - 1)

-- instance Arbitrary (CodeAttributes High) where
--   arbitrary = genericArbitraryU

instance Arbitrary (ExceptionTable High) where
  arbitrary =
    ExceptionTable <$> arbitraryRef 0xffff <*> arbitraryRef 0xffff <*> arbitraryRef 0xffff <*> arbitrary

instance Arbitrary (ByteCode High) where
  arbitrary = do
    s <- getSize
    ByteCode 0 . V.map (ByteCodeInst 0) . V.fromList <$> vectorOf s (genByteCodeOpr s)

instance Arbitrary (ByteCodeInst High) where
  arbitrary =
    ByteCodeInst 0 <$> genByteCodeOpr 0xffff

instance Arbitrary ArithmeticType where
  arbitrary = elements [MInt, MLong, MFloat, MDouble]

instance Arbitrary LocalType where
  arbitrary = elements [LInt, LLong, LFloat, LDouble, LRef]

genByteCodeOpr :: Int -> Gen (ByteCodeOpr High)
genByteCodeOpr i = do
  x <- (genericArbitraryU :: Gen (ByteCodeOpr High))
  case x of
    If cp _ -> If cp <$> arbitraryRef i
    IfZ b _ -> IfZ b <$> arbitraryRef i
    Goto _ -> Goto <$> arbitraryRef i
    Jsr _ -> Jsr <$> arbitraryRef i
    TableSwitch _ (SwitchTable l ofss) ->
      TableSwitch
        <$> arbitraryRef i
        <*> (SwitchTable l <$> V.mapM (const $ arbitraryRef i) ofss)
    LookupSwitch _ ofss ->
      LookupSwitch
        <$> arbitraryRef i
        <*> V.mapM (\(a, _) -> (a,) <$> arbitraryRef i) ofss
    _ -> return x

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance Arbitrary ArrayType where
  arbitrary = genericArbitrary uniform

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

instance Arbitrary NewArrayType where
  arbitrary = genericArbitraryU `suchThat` (\(NewArrayType i _) -> i > 0)

instance Arbitrary SmallArithmeticType where
  arbitrary = genericArbitraryU

instance Arbitrary CmpOpr where
  arbitrary = genericArbitraryU

instance Arbitrary FieldAccess where
  arbitrary = genericArbitrary uniform

instance Arbitrary (Invocation High) where
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

instance Arbitrary (SwitchTable High) where
  arbitrary = genericArbitraryU
