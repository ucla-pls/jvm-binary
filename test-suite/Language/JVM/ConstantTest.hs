{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.ConstantTest where

import SpecHelper

import Language.JVM.Constant
import Language.JVM.UtilsTest ()

import qualified Data.IntMap as IM

prop_encode_and_decode :: ConstantPool -> Property
prop_encode_and_decode = isoBinary

instance Arbitrary ConstantRef where
  arbitrary =
    ConstantRef <$> arbitrary

instance Arbitrary ConstantPool where
  arbitrary =
    ConstantPool . IM.fromList . go 1 <$> arbitrary
    where
      go n (e : lst) =
        (n, e) : go (n + constantSize e) lst
      go _ [] = []

instance Arbitrary Constant where
  arbitrary = oneof
    [ String <$> arbitrary
    , Integer <$> arbitrary
    , Float <$> arbitrary
    , Long <$> arbitrary
    , Double <$> arbitrary
    , ClassRef <$> arbitrary
    , StringRef <$> arbitrary
    , FieldRef <$> arbitrary <*> arbitrary
    , MethodRef <$> arbitrary <*> arbitrary
    , InterfaceMethodRef <$> arbitrary <*> arbitrary
    , NameAndType <$> arbitrary <*> arbitrary
    , MethodHandle <$> arbitrary <*> arbitrary
    , MethodType <$> arbitrary
    , InvokeDynamic <$> arbitrary <*> arbitrary
    ]
