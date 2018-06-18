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

  it "works on chinese characters" $ do
    tryDecode (TE.encodeUtf8 "试一试中文") `shouldBe` Right "试一试中文"

  -- xit "works on http:/foo/p\237\160\128" $ do
  --   -- I don't know exactly the right result of this string, but it should be a correct java byte-string
  --   sizedByteStringToText "http:/foo/p\237\160\128" `shouldBe` Right "http:/foo/p?"

  -- xit "works on long and obscure byte string" $ do
  --   -- I don't know exactly the right result of this string, but it should be a correct java byte-string
  --   sizedByteStringToText obscureByteString `shouldBe` Right "?"

obscureByteString :: SizedByteString a
obscureByteString = SizedByteString
    "\192\128\DEL\194\128\195\191\196\128\197\191\198\128\201\143\201\144\202\175\202\176\203\191\204\128\205\175\205\176\207\191\224\144\128\224\147\191\224\148\176\224\150\143\224\150\144\224\151\191\224\152\128\224\155\191\224\156\128\224\157\143\224\158\128\224\158\191\224\164\128\224\165\191\224\166\128\224\167\191\224\168\128\224\169\191\224\170\128\224\171\191\224\172\128\224\173\191\224\174\128\224\175\191\224\176\128\224\177\191\224\178\128\224\179\191\224\180\128\224\181\191\224\182\128\224\183\191\224\184\128\224\185\191\224\186\128\224\187\191\224\188\128\224\191\191\225\128\128\225\130\159\225\130\160\225\131\191\225\132\128\225\135\191\225\136\128\225\141\191\225\142\160\225\143\191\225\144\128\225\153\191\225\154\128\225\154\159\225\154\160\225\155\191\225\158\128\225\159\191\225\160\128\225\162\175\225\184\128\225\187\191\225\188\128\225\191\191\226\128\128\226\129\175\226\129\176\226\130\159\226\130\160\226\131\143\226\131\144\226\131\191\226\132\128\226\133\143\226\133\144\226\134\143\226\134\144\226\135\191\226\136\128\226\139\191\226\140\128\226\143\191\226\144\128\226\144\191\226\145\128\226\145\159\226\145\160\226\147\191\226\148\128\226\149\191\226\150\128\226\150\159\226\150\160\226\151\191\226\152\128\226\155\191\226\156\128\226\158\191\226\160\128\226\163\191\226\186\128\226\187\191\226\188\128\226\191\159\226\191\176\226\191\191\227\128\128\227\128\191\227\129\128\227\130\159\227\130\160\227\131\191\227\132\128\227\132\175\227\132\176\227\134\143\227\134\144\227\134\159\227\134\160\227\134\191\227\136\128\227\139\191\227\140\128\227\143\191\227\144\128\228\182\181\228\184\128\233\191\191\234\128\128\234\146\143\234\146\144\234\147\143\234\176\128\237\158\163\238\128\128\239\163\191\239\164\128\239\171\191\239\172\128\239\173\143\239\173\144\239\183\191\239\184\160\239\184\175\239\184\176\239\185\143\239\185\144\239\185\175\239\185\176\239\187\190\239\187\191\239\187\191\239\188\128\239\191\175"


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
