{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.AttributeTest where

import SpecHelper

import Language.JVM.Attribute (Attribute (..))
import Language.JVM.ConstantTest ()
import Language.JVM.Constant (Index )
import Language.JVM.Utils
import Language.JVM.UtilsTest ()

import qualified Data.ByteString as BS

prop_encode_and_decode :: Attribute Index -> Property
prop_encode_and_decode = isoBinary

instance Arbitrary (Attribute Index) where
  arbitrary = do
    idx <- arbitrary
    len <- choose (0, 50)
    bs <- SizedByteString . BS.pack <$> sequence (replicate len arbitrary)
    return $ Attribute idx bs