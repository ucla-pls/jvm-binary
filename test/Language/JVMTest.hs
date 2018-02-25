module Language.JVMTest where

import SpecHelper

import Data.Either
import Data.Foldable
import Language.JVM
import qualified Language.JVM.Attribute.Code as C

test_reading_classfile :: IO [TestTree]
test_reading_classfile = testSomeFiles $ do
  it "can parse the bytestring" $ \bs -> do
    decodeClassFile bs `shouldSatisfy` isRight

  beforeWith (\bs -> either (fail . show) return $ decodeClassFile bs) $ do

    it "has a the magic number: 0xCAFEBABE" $ \cls ->
      cMagicNumber cls `shouldBe` 0xCAFEBABE

    it "can bootstrap the constant pool" $ \cls -> do
      let
        -- cls :: ClassFile Low
        -- cls = cls_
        cp :: Either ClassFileError (ConstantPool High)
        cp = bootstrapConstantPool (cConstantPool cls)
      cp `shouldSatisfy` isRight
      let Right cp' = cp
      forM_ (cMethods' cls) $ \m -> do
        case (runEvolve cp' (evolve m)) of
          Right m -> return ()
          Left err -> do
            putStr (show err) >> putStr ": "
            print . runEvolve cp' $ do
              x <- evolve (mDescriptorIndex m)
              n <- evolve (mNameIndex m)
              return (n, x)
            forM_ (mAttributes m) $ \a -> do
              -- Assume code
              case fromAttribute' a of
                Right c -> do
                  forM_ (C.unByteCode . C.codeByteCode $ c) $ \i ->
                    putStr " -> " >> print i

                  forM_ (C.codeAttributes c) $ \ca -> do
                    print $ runEvolve cp' (evolve ca)
                    putStrLn (hexStringS $ aInfo ca)
                    case fromAttribute' ca :: Either String (StackMapTable Low) of
                      Right x ->
                        print x
                      Left msg ->
                        print msg
                Left x ->
                  print x

    it "can untie the whole class file" $ \cls -> do
      evolveClassFile cls `shouldSatisfy` isRight

    -- it "has a class name" $ \cls ->
    --   runWithPool (cThisClass cls) (cConstantPool cls) `shouldSatisfy` isRight

    -- it "can parse all method codes" $ \cls ->
    --   case runWithPool (mapM mCode $ cMethods cls) (cConstantPool cls) of
    --     Left a -> expectationFailure (show a)
    --     Right rs ->
    --       forM_ (catMaybes rs) $ \code ->
    --         code `shouldSatisfy` isRight
