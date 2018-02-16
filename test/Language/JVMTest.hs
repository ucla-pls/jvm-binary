module Language.JVMTest where

import SpecHelper

import Data.Either
import Language.JVM
import Language.JVM.Attribute.Code ()

test_reading_classfile :: IO [TestTree]
test_reading_classfile = testSomeFiles $ do
  it "can parse the bytestring" $ \bs -> do
    decodeClassFile bs `shouldSatisfy` isRight

  beforeWith (\bs -> either (fail . show) return $ decodeClassFile bs) $ do

    it "has a the magic number: 0xCAFEBABE" $ \cls ->
      cMagicNumber cls `shouldBe` 0xCAFEBABE

    it "can bootstrap the constant pool" $ \cls -> do
      let cp = bootstrapDeref (cConstantPool cls)
      cp `shouldSatisfy` isRight

    it "can untie the whole class file" $ \cls -> do
      let cls' = untieClassFile cls
      untieClassFile cls `shouldSatisfy` isRight

    -- it "has a class name" $ \cls ->
    --   runWithPool (cThisClass cls) (cConstantPool cls) `shouldSatisfy` isRight

    -- it "can parse all method codes" $ \cls ->
    --   case runWithPool (mapM mCode $ cMethods cls) (cConstantPool cls) of
    --     Left a -> expectationFailure (show a)
    --     Right rs ->
    --       forM_ (catMaybes rs) $ \code ->
    --         code `shouldSatisfy` isRight
