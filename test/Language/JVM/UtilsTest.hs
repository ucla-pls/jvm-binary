{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.JVM.UtilsTest where

import SpecHelper

import qualified Data.ByteString as BS

import Data.Set as Set
import qualified Data.Text.Encoding as TE
import qualified Data.Text as Text
import Language.JVM.Utils

prop_test_our_zero_decoder :: Text.Text -> Bool
prop_test_our_zero_decoder a =
   tryDecode (TE.encodeUtf8 a) == Right a

spec_parse_zero_text :: SpecWith ()
spec_parse_zero_text = do
  it "can read zero" $ do
    sizedByteStringToText "\192\128" `shouldBe` Right "\0"

  it "can read zero padded with text" $ do
    sizedByteStringToText "Some text \192\128 a x" `shouldBe` Right "Some text \0 a x"

  it "can convert a zero back again" $ do
    sizedByteStringFromText "\0" `shouldBe` "\192\128"

  it "can convert a padded zero back again" $ do
    sizedByteStringFromText "Some text \0 a x" `shouldBe` "Some text \192\128 a x"

  it "works on wierd strings" $ do
    tryDecode (TE.encodeUtf8 "\0  asd ßåæ∂ø∆œ˜˜¬å˚¬") `shouldBe` Right "\0  asd ßåæ∂ø∆œ˜˜¬å˚¬"

instance Arbitrary a => Arbitrary (SizedList w a) where
  arbitrary =
    SizedList <$> arbitrary
  shrink (SizedList c) =
    SizedList <$> shrink c

instance Arbitrary (SizedByteString w) where
  arbitrary = do
    len <- choose (0, 50)
    SizedByteString . BS.pack <$> sequence (replicate len arbitrary)

instance (Enumish a) => Arbitrary (BitSet w a) where
  arbitrary =
    BitSet . Set.fromList <$> sublistOf (snd <$> inOrder)
