{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.JVM.AttributeSpec where

import           SpecHelper

import           Language.JVM

import           Language.JVM.Attribute    (Attribute (..))
import           Language.JVM.ConstantSpec ()
import           Language.JVM.UtilsSpec    ()

import qualified Data.ByteString           as BS

spec :: Spec
spec = do
 it "roundtrip" $ property $ (isoRoundtrip :: Attribute High -> Property)

instance Arbitrary (Attribute High) where
  arbitrary = do
    _idx <- arbitrary
    len <- choose (0, 50)
    bs <- SizedByteString . BS.pack <$> sequence (replicate len arbitrary)
    return $ Attribute _idx bs
