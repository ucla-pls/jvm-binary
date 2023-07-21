{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JVM.ConstantSpec where

import GHC.Generics
import SpecHelper

import Generic.Random.Internal.Generic

import qualified Data.ByteString as BS
import qualified Data.Text as Text

import Language.JVM
import Language.JVM.TypeSpec ()
import Language.JVM.UtilsSpec ()

spec :: Spec
spec = do
  it "can print things correctly" $ do
    show ("java/lang/Object.hello:()V" :: AbsMethodId)
      `shouldBe` (show ("java/lang/Object.hello:()V" :: String))

  it "can print things correctly" $ do
    show ("java/lang/Object.hello:I" :: AbsFieldId)
      `shouldBe` (show ("java/lang/Object.hello:I" :: String))

  it "can print things correctly" $ do
    show ("hello:()V" :: MethodId)
      `shouldBe` (show ("hello:()V" :: String))

  it "can print things correctly" $ do
    show ("hello:I" :: FieldId)
      `shouldBe` (show ("hello:I" :: String))

  it "can build a class pool" $ do
    let
      (a', cpb) = runConstantPoolBuilder (devolve (CClassRef "class/Name")) cpbEmpty
      cp = constantPoolFromBuilder cpb
    cp `shouldBe` fromConstants [CString "class/Name"]
    a' `shouldBe` (CClassRef 1)

    let cp' = bootstrapConstantPool cp
    cp' `shouldBe` Right (fromConstants [CString "class/Name"])

  it "can build a complex class pool" $ do
    let
      a = CMethodRef (InRefType "Lclass/Name;" "method:()V")
      (a', cpb) = runConstantPoolBuilder (devolve a) cpbEmpty
      cp = constantPoolFromBuilder cpb
    cp
      `shouldBe` fromConstants
        [ CString "class/Name"
        , CClassRef 1
        , CString "method"
        , CString "()V"
        , CNameAndType 3 4
        ]
    a' `shouldBe` (CMethodRef (2, 5))

    let cp' = bootstrapConstantPool cp
    cp'
      `shouldBe` Right
        ( fromConstants
            [ CString "class/Name"
            , CClassRef "class/Name"
            , CString "method"
            , CString "()V"
            , CNameAndType "method" "()V"
            ]
        )

    let Right cp'' = cp'
    runEvolve (EvolveConfig [] cp'' (const True)) (evolve a') `shouldBe` Right a

  it "can encode and decode" $
    property $
      (isoBinary :: ConstantPool Low -> Property)
  it "can do a roundtrip" $
    property $
      (isoRoundtrip :: Constant High -> Property)

instance Arbitrary (ConstantPool Low) where
  arbitrary = do
    lst <- arbitrary :: Gen [Constant High]
    let (_, x) = runConstantPoolBuilder (mapM devolve lst) cpbEmpty
    return (constantPoolFromBuilder x)

instance Arbitrary Text.Text where
  arbitrary =
    elements
      [ "Package"
      , "test"
      , "number"
      , "stuff"
      , "\0  asd ßåæ∂ø∆œ˜˜¬å˚¬"
      ]

instance Arbitrary BS.ByteString where
  arbitrary =
    elements
      [ "Package"
      , "test"
      , "number"
      , "stuff"
      , "\0  asd ßåæ∂ø∆œ˜˜¬å˚¬"
      ]

instance Arbitrary (ConstantPool High) where
  arbitrary =
    fromConstants <$> (arbitrary :: Gen [Constant High])

instance Arbitrary AbsInterfaceMethodId where
  arbitrary = genericArbitraryU

instance Arbitrary AbsVariableMethodId where
  arbitrary = genericArbitraryU

instance Arbitrary (Constant High) where
  arbitrary = sized $ \n ->
    if n < 2
      then
        oneof
          [ CString <$> arbitrary
          , CInteger <$> arbitrary
          , CFloat <$> arbitrary
          , CLong <$> arbitrary
          , CDouble <$> arbitrary
          ]
      else
        scale (flip div 2) $
          oneof
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

instance Arbitrary (MethodHandle High) where
  arbitrary =
    oneof
      [ MHField <$> (MethodHandleField <$> arbitrary <*> arbitrary)
      , MHMethod <$> arbitrary
      , MHInterface <$> (MethodHandleInterface <$> arbitrary)
      ]

instance Arbitrary (MethodHandleMethod High) where
  arbitrary =
    oneof
      [ MHInvokeVirtual <$> arbitrary
      , MHInvokeStatic <$> arbitrary
      , MHInvokeSpecial <$> arbitrary
      , MHNewInvokeSpecial <$> arbitrary
      ]

instance Arbitrary MethodHandleFieldKind where
  arbitrary =
    oneof [pure x | x <- [MHGetField, MHGetStatic, MHPutField, MHPutStatic]]

-- instance Arbitrary (AbsVariableMethodId High) where
--   arbitrary = genericArbitraryU

instance Arbitrary (InvokeDynamic High) where
  arbitrary = InvokeDynamic <$> arbitrary <*> arbitrary

instance Arbitrary JValue where
  arbitrary = genericArbitraryU
