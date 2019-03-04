{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JVM.Attribute.LineNumberTableSpec where

import qualified Data.IntMap                as IM

import           SpecHelper

import           Language.JVM.AttributeSpec ()
import           Language.JVM.ConstantSpec  ()
import           Language.JVM.TypeSpec      ()
import           Language.JVM.UtilsSpec     ()

import           Language.JVM
import           Language.JVM.Attribute.LineNumberTable

spec :: Spec
spec = return ()

-- prop_roundtrip_LineNumberTable :: LineNumberTable High -> Property
-- prop_roundtrip_LineNumberTable = isoRoundtrip

instance Arbitrary (LineNumberTable High) where
  arbitrary =
    LineNumberTable . IM.fromList . map f . unSizedList <$> (arbitrary :: Gen BinaryFormat)
    where
      f (a,b) = (fromIntegral a, b)
