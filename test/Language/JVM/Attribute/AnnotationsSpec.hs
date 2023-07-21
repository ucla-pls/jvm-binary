{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--  |
module Language.JVM.Attribute.AnnotationsSpec where

import SpecHelper

import Language.JVM
import Language.JVM.Attribute.Annotations
import Language.JVM.ConstantSpec ()

spec :: Spec
spec =
  return ()

--   prop "annotation can do a roundtrip" prop_roundtrip_Annotations
--
-- prop_roundtrip_Annotations :: Annotation High -> Property
-- prop_roundtrip_Annotations = isoRoundtrip
--
-- instance Arbitrary (Annotation High) where
--   arbitrary = genericArbitraryU
--
-- instance Arbitrary (ValuePair High) where
--   arbitrary = genericArbitraryU
--
-- instance Arbitrary (ElementValue High) where
--   arbitrary = oneof
--     [ EByte   <$> arbitrary
--     , EChar   <$> arbitrary
--     , EDouble <$> arbitrary
--     , EFloat  <$> arbitrary
--     , EInt    <$> arbitrary
--     , ELong   <$> arbitrary
--     , EShort  <$> arbitrary
--     , EString <$> arbitrary
--
--     , EEnum   <$> arbitrary
--     , EClass  <$> arbitrary
--     ]
--
-- instance Arbitrary (EnumValue High) where
--   arbitrary = genericArbitraryU

-- instance Arbitrary (BootstrapMethod High) where
--   arbitrary =
--     BootstrapMethod <$> arbitrary <*> (SizedList <$> pure [])
