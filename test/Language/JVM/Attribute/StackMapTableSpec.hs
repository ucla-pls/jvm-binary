{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JVM.Attribute.StackMapTableSpec where

import           SpecHelper

import           Data.Bifunctor
import           Data.Binary
import qualified Data.ByteString.Lazy                 as BL
import           Data.Either

import           Language.JVM.Attribute.StackMapTable
import           Language.JVM

import           Language.JVM.AttributeSpec           ()
import           Language.JVM.ConstantSpec            ()
import           Language.JVM.TypeSpec                ()

-- prop_encode_and_decode :: StackMapTable Low -> Property
-- prop_encode_and_decode = isoBinary

spec :: SpecWith ()
spec = do
  describe "decoding" $ do
    let
      bs = BL.fromStrict "\NUL\ACK\253\NUL\t\SOH\SOH\253\NUL%\SOH\SOH\t\ETB\249\NUL\t\249\NUL\r"
    -- 0006fd00 090101fd 00250101 0917f900 09f9000d
    it ("can decode " ++ hexString bs) $ do
      let
        r :: Either String (StackMapTable Low)
        r = bimap trd trd $ decodeOrFail bs

      r `shouldSatisfy` isRight

  describe "offsetDelta" $ do
    it "uphold that offesetDelta -| offsetDeltaInv" $ property $ prop_offset_delta

-- prop_offset_delta :: (Word16, Word16) -> Bool
prop_offset_delta :: (Word16, Word16) -> Property
prop_offset_delta (lidx, delta) =
  let tidx = offsetDelta lidx delta
  in counterexample (show tidx) $
     offsetDeltaInv lidx tidx === delta

-- prop_roundtrip_StackMapTable :: StackMapTable High -> Property
-- prop_roundtrip_StackMapTable = isoRoundtrip

-- prop_roundtrip_StackMapFrame :: StackMapFrame High -> Property
-- prop_roundtrip_StackMapFrame = isoRoundtrip

instance Arbitrary (StackMapTable High) where
  arbitrary = StackMapTable <$> arbitrary

instance Arbitrary ( StackMapFrame High) where
  arbitrary =
    StackMapFrame <$> arbitrary <*> arbitrary

instance Arbitrary (StackMapFrameType High) where
  arbitrary = oneof
    [ pure SameFrame
    , SameLocals1StackItemFrame <$> arbitrary
    , ChopFrame <$> choose (1,3)
    , AppendFrame <$> (choose (1,3) >>= flip vectorOf arbitrary)
    , FullFrame <$> arbitrary <*> arbitrary
    ]

instance Arbitrary (VerificationTypeInfo High) where
  arbitrary = oneof
    [ pure VTTop
    , pure VTInteger
    , pure VTFloat
    , pure VTLong
    , pure VTDouble
    , pure VTNull
    , pure VTUninitializedThis
    , VTObject <$> arbitrary
    , VTUninitialized <$> arbitrary
    ]
