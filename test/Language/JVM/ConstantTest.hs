{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.JVM.ConstantTest where

import SpecHelper

import qualified Data.IntMap as IM
import qualified Data.Text as Text

import Language.JVM.Constant
import Language.JVM.ConstantPool
import Language.JVM.UtilsTest ()
import Language.JVM.TypeTest ()

-- prop_encode_and_decode :: ConstantPool Low -> Property
-- prop_encode_and_decode = isoBinary

-- prop_Constant_encode_and_decode :: Constant Low -> Property
-- prop_Constant_encode_and_decode = isoBinary

prop_Constant_roundtrip :: Constant High -> Property
prop_Constant_roundtrip = isoRoundtrip

instance Arbitrary Text.Text where
  arbitrary =
    elements
    [ "Package"
    , "test"
    , "number"
    , "stuff"
    ]

instance Arbitrary a => Arbitrary (Ref a High) where
  arbitrary =
    RefV <$> arbitrary
  shrink (RefV x) =
    RefV <$> shrink x

instance Arbitrary (a High) => Arbitrary (DeepRef a High) where
  arbitrary =
    DeepRef . RefV <$> arbitrary
  shrink (DeepRef (RefV x)) =
    DeepRef .RefV <$> shrink x

instance Arbitrary (ConstantPool High) where
  arbitrary =
    ConstantPool . IM.fromList . go 1 <$> arbitrary
    where
      go n (e : lst) =
        (n, e) : go (n + constantSize e) lst
      go _ [] = []

instance Arbitrary (InterfaceMethod High) where
  arbitrary = genericArbitraryU

instance Arbitrary (Constant High) where
  arbitrary = oneof
    [ CString <$> arbitrary
    , CInteger <$> arbitrary
    , CFloat <$> arbitrary
    , CLong <$> arbitrary
    , CDouble <$> arbitrary
    , CClassRef <$> arbitrary
    , CStringRef <$> arbitrary
    , CFieldRef <$> arbitrary
    , CMethodRef <$> arbitrary
    , CInterfaceMethodRef <$> arbitrary
    , CNameAndType <$> arbitrary <*> arbitrary
    , CMethodHandle <$> arbitrary
    , CMethodType <$> arbitrary
    , CInvokeDynamic <$> arbitrary
    ]

instance (Arbitrary (a High)) => Arbitrary (InClass a High) where
  arbitrary = InClass <$> arbitrary <*> arbitrary

instance Arbitrary (FieldId High) where
  arbitrary = FieldId <$> arbitrary <*> arbitrary

instance Arbitrary (MethodId High) where
  arbitrary = MethodId <$> arbitrary <*> arbitrary

instance Arbitrary (MethodHandle High) where
  arbitrary =
    oneof
      [ MHField <$> ( MethodHandleField <$> arbitrary <*> arbitrary)
      , MHMethod <$> ( MethodHandleMethod <$> arbitrary <*> arbitrary)
      , MHInterface <$> ( MethodHandleInterface <$> arbitrary)
      ]

instance Arbitrary MethodHandleFieldKind where
  arbitrary =
    oneof [ pure x | x <- [ MHGetField, MHGetStatic, MHPutField, MHPutStatic ] ]

instance Arbitrary MethodHandleMethodKind where
  arbitrary =
    oneof [ pure x | x <- [ MHInvokeVirtual , MHInvokeStatic , MHInvokeSpecial , MHNewInvokeSpecial ] ]

instance Arbitrary (InvokeDynamic High) where
  arbitrary = InvokeDynamic <$> arbitrary <*> arbitrary
