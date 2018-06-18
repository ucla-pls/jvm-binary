{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JVM.Attribute.LineNumberTableTest where

import qualified Data.IntMap                as IM

import           SpecHelper

import           Language.JVM.AttributeTest ()
import           Language.JVM.ConstantTest  ()
import           Language.JVM.TypeTest      ()
import           Language.JVM.UtilsTest     ()

import           Language.JVM
import           Language.JVM.Attribute.LineNumberTable

-- prop_roundtrip_LineNumberTable :: LineNumberTable High -> Property
-- prop_roundtrip_LineNumberTable = isoRoundtrip

instance Arbitrary (LineNumberTable High) where
  arbitrary =
    LineNumberTable . IM.fromList . map f . unSizedList <$> (arbitrary :: Gen BinaryFormat)
    where
      f (a,b) = (fromIntegral a, b)
