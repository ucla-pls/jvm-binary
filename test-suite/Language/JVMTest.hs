module Language.JVMTest where

import SpecHelper

import Control.Monad
-- import Data.Text as Text
import Data.Maybe
import Data.Either
-- import Debug.Trace

import Language.JVM
import Language.JVM.Attribute.Code ()

test_reading_classfile :: IO [TestTree]
test_reading_classfile = testAllFiles $ do
  it "can parse the bytestring" $ \bs -> do
    decodeClassFile bs `shouldSatisfy` isRight

  beforeWith (\bs -> either fail return $ decodeClassFile bs) $ do

    it "has a the magic number: 0xCAFEBABE" $ \cls ->
      cMagicNumber cls `shouldBe` 0xCAFEBABE

    it "has a class name" $ \cls ->
      cThisClass (cConstantPool cls) cls `shouldSatisfy` isJust

    it "can parse all method codes" $ \cls ->
      forM_ (catMaybes . map (mCode (cConstantPool cls)) $ cMethods cls) $
      \code-> do
        code `shouldSatisfy` isRight
