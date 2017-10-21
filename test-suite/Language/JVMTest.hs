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
test_reading_classfile = testSomeFiles $ do
  it "can parse the bytestring" $ \bs -> do
    decodeClassFile bs `shouldSatisfy` isRight

  beforeWith (\bs -> either fail return $ decodeClassFile bs) $ do

    it "has a the magic number: 0xCAFEBABE" $ \cls ->
      cMagicNumber cls `shouldBe` 0xCAFEBABE

    it "has a class name" $ \cls ->
      runWithPool (cThisClass cls) (cConstantPool cls) `shouldSatisfy` isRight

    it "can parse all method codes" $ \cls ->
      case runWithPool (mapM mCode $ cMethods cls) (cConstantPool cls) of
        Left a -> expectationFailure (show a)
        Right rs ->
          forM_ (catMaybes rs) $ \code ->
            code `shouldSatisfy` isRight
