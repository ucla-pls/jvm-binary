{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JVM.Attribute.LineNumberTableTest where

import           SpecHelper

import qualified Data.IntMap as IM
import           Language.JVM.AttributeTest           ()
import           Language.JVM.ConstantTest            ()
import           Language.JVM.UtilsTest               ()
import           Language.JVM.TypeTest               ()

import           Language.JVM.Constant
import           Language.JVM.Utils

import           Language.JVM.Attribute.LineNumberTable

prop_roundtrip_LineNumberTable :: LineNumberTable High -> Property
prop_roundtrip_LineNumberTable = isoRoundtrip

instance Arbitrary (LineNumberTable High) where
  arbitrary =
    LineNumberTable . IM.fromList . map f . unSizedList <$> (arbitrary :: Gen BinaryFormat)
    where
      f (a,b) = (fromIntegral a, b)
