{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.AttributeTest where

import           SpecHelper

import           Language.JVM

import           Language.JVM.Attribute    (Attribute (..))
import           Language.JVM.ConstantTest ()
import           Language.JVM.UtilsTest    ()

import qualified Data.ByteString           as BS

prop_roundtrip_Attribute :: Attribute High -> Property
prop_roundtrip_Attribute = isoRoundtrip

instance Arbitrary (Attribute High) where
  arbitrary = do
    _idx <- arbitrary
    len <- choose (0, 50)
    bs <- SizedByteString . BS.pack <$> sequence (replicate len arbitrary)
    return $ Attribute _idx bs
