{-# LANGUAGE OverloadedStrings #-}
module Language.JVMTest where

import SpecHelper

import Data.Either
import qualified Data.IntMap as IM
import qualified Data.List as L

import Data.Foldable
import Language.JVM
import qualified Language.JVM.Attribute.Code as C
import qualified Data.ByteString.Lazy as BL

import Data.Binary


spec_testing_example :: SpecWith ()
spec_testing_example =
  it "can read classfile from file" $ do
    eclf <- readClassFile <$> BL.readFile "test/data/project/Main.class"
    case eclf of
      Right clf -> do
        cThisClass clf `shouldBe` ClassName "Main"
        cSuperClass clf `shouldBe` ClassName "java/lang/Object"
      Left msg ->
        fail $ show msg

test_reading_classfile :: IO [TestTree]
test_reading_classfile = testAllFiles $ do
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
          Right _ -> return ()
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

    it "can encode/decode the class file" $ \cls -> do
      let e = encodeClassFile cls
      decodeClassFile e `shouldBe` Right cls

    it "can evolve the whole class file" $ \cls -> do
      let me = evolveClassFile cls
      me `shouldSatisfy` isRight

    it "has same or smaller constant pool" $ \cls -> do
      let Right x = evolveClassFile cls
      let d = devolveClassFile x
      -- forM_ (L.sort . IM.elems . unConstantPool $ cConstantPool d) print
      (IM.size . unConstantPool $ cConstantPool d) `shouldSatisfy`
         (<= (IM.size . unConstantPool $ cConstantPool cls))

    it "can do full read - write - read process" $ \cls -> do
      let Right x = evolveClassFile cls
      let y' = readClassFile (writeClassFile x)
      y' `shouldSatisfy` isRight
      let Right y = y'
      cAccessFlags' y `shouldBe` cAccessFlags' x
      cThisClassIndex y `shouldBe` cThisClassIndex x
      cSuperClassIndex y `shouldBe` cSuperClassIndex x
      cInterfaceIndicies' y `shouldBe` cInterfaceIndicies' x
      cFields' y `shouldBe` cFields' x
      forM_ (zip (cMethods y) (cMethods x)) $ \ (ym, xm) -> do
        ym `shouldMatchMethod` xm

    -- it "has a class name" $ \cls ->
    --   runWithPool (cThisClass cls) (cConstantPool cls) `shouldSatisfy` isRight

    -- it "can parse all method codes" $ \cls ->
    --   case runWithPool (mapM mCode $ cMethods cls) (cConstantPool cls) of
    --     Left a -> expectationFailure (show a)
    --     Right rs ->
    --       forM_ (catMaybes rs) $ \code ->
    --         code `shouldSatisfy` isRight

shouldMatchMethod ym xm = do
  mAccessFlags ym `shouldBe` mAccessFlags xm
  mName ym `shouldBe` mName xm
  mDescriptor ym `shouldBe` mDescriptor xm
  mExceptions ym `shouldMatchList` mExceptions xm
  case (mCode ym, mCode xm) of
    (Just yc, Just xc) -> do
      forM_ (zip (C.codeByteCodeOprs yc) (C.codeByteCodeOprs xc)) $ \(yb,yc) -> do
        yb `shouldBe` yc
    _ -> mCode ym `shouldBe` mCode xm
