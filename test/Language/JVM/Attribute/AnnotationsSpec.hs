{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
--  |
module Language.JVM.Attribute.AnnotationsSpec where

import           SpecHelper

import           Language.JVM.ConstantSpec               ()
import           Language.JVM
import           Language.JVM.Attribute.Annotations

spec :: Spec
spec =
  prop "annotation can do a roundtrip" prop_roundtrip_Annotations

prop_roundtrip_Annotations :: Annotation High -> Property
prop_roundtrip_Annotations = isoRoundtrip

instance Arbitrary (Annotation High) where
  arbitrary = genericArbitraryU

instance Arbitrary (ValuePair High) where
  arbitrary = genericArbitraryU

instance Arbitrary (ElementValue High) where
  arbitrary = oneof
    [ EByte   <$> arbitrary
    , EChar   <$> arbitrary
    , EDouble <$> arbitrary
    , EFloat  <$> arbitrary
    , EInt    <$> arbitrary
    , ELong   <$> arbitrary
    , EShort  <$> arbitrary
    , EString <$> arbitrary

    , EEnum   <$> arbitrary
    , EClass  <$> arbitrary
    ]

instance Arbitrary (EnumValue High) where
  arbitrary = genericArbitraryU


-- instance Arbitrary (BootstrapMethod High) where
--   arbitrary =
--     BootstrapMethod <$> arbitrary <*> (SizedList <$> pure [])
