{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.AttributeTest where

import SpecHelper

import Language.JVM.Attribute (Attribute (..))
import Language.JVM.ConstantTest ()
import Language.JVM.Utils
import Language.JVM.UtilsTest ()

import qualified Data.ByteString as BS

prop_encode_and_decode :: Attribute -> Property
prop_encode_and_decode = isoBinary

instance Arbitrary Attribute where
  arbitrary = do
    idx <- arbitrary
    len <- choose (0, 50)
    bs <- SizedByteString . BS.pack <$> sequence (replicate len arbitrary)
    return $ Attribute idx bs
