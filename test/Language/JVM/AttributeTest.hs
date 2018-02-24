{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.AttributeTest where

import SpecHelper

import Language.JVM.Attribute (Attribute (..))
import Language.JVM.ConstantTest ()
import Language.JVM.Constant
import Language.JVM.Utils
import Language.JVM.UtilsTest ()

import qualified Data.ByteString as BS

prop_roundtrip :: Attribute High -> Property
prop_roundtrip = isoRoundtrip

instance Arbitrary (Attribute High) where
  arbitrary = do
    _idx <- arbitrary
    len <- choose (0, 50)
    bs <- SizedByteString . BS.pack <$> sequence (replicate len arbitrary)
    return $ Attribute _idx bs
