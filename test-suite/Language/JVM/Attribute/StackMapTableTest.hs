{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JVM.Attribute.StackMapTableTest where

import           SpecHelper

import           Language.JVM.AttributeTest           ()
import           Language.JVM.ConstantTest            ()
import           Language.JVM.UtilsTest               ()

import           Language.JVM.Constant

import           Language.JVM.Attribute.StackMapTable

prop_encode_and_decode :: StackMapTable Index -> Property
prop_encode_and_decode = isoBinary

prop_StackMapFrame_encode_and_decode :: StackMapFrame Index -> Property
prop_StackMapFrame_encode_and_decode = isoBinary

instance Arbitrary (StackMapTable Index) where
  arbitrary = StackMapTable <$> arbitrary

instance Arbitrary ( StackMapFrame Index) where
  arbitrary =
    StackMapFrame <$> arbitrary <*> arbitrary

instance Arbitrary (StackMapFrameType Index) where
  arbitrary = oneof
    [ pure SameFrame
    , SameLocals1StackItemFrame <$> arbitrary
    , ChopFrame <$> choose (1,3)
    , AppendFrame <$> (choose (1,3) >>= flip vectorOf arbitrary)
    , FullFrame <$> arbitrary <*> arbitrary
    ]

instance Arbitrary (VerificationTypeInfo Index) where
  arbitrary = oneof
    [ pure VTop
    , pure VInteger
    , pure VTop
    , pure VInteger
    , pure VFloat
    , pure VLong
    , pure VDouble
    , pure VNull
    , pure VUninitializedThis
    , VObject <$> arbitrary
    , VUninitialized <$> arbitrary
    ]
