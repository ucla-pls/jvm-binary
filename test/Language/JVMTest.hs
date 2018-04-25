{-# LANGUAGE OverloadedStrings #-}
module Language.JVMTest where

import SpecHelper

import Data.Either
import Data.List as List
import qualified Data.IntMap as IM
-- import qualified Data.List as L

import Data.Foldable
import Language.JVM
import qualified Language.JVM.Attribute.Code as C
import Language.JVM.Attribute.StackMapTable
import qualified Data.ByteString.Lazy as BL

-- import Data.Binary


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
test_reading_classfile = testAllFiles $ \bs -> do
  let d = decodeClassFile bs
  it "can parse the bytestring" $ do
    d `shouldSatisfy` isRight

  let Right cls = d
  it "has a the magic number: 0xCAFEBABE" $ do
    cMagicNumber cls `shouldBe` 0xCAFEBABE

  it "can bootstrap the constant pool" $ do
    let
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

  describe "encoding/decoding" $ do
    let e = encodeClassFile cls
    it "should encode to the original bytestring" $
      e `shouldBe` bs

    it "should decode to the same thing" $
      decodeClassFile e `shouldBe` Right cls

  describe "evolving/devolving" $ do
    let me = evolveClassFile cls
    it "can evolve the whole class file" $ do
      me `shouldSatisfy` isRight

    let Right x = me
    it "has same or smaller constant pool" $ do
      let d = devolveClassFile x
      (IM.size . unConstantPool $ cConstantPool d) `shouldSatisfy`
        (<= (IM.size . unConstantPool $ cConstantPool cls))

    it "is the same when devolving with the original constant pool" $
      devolveClassFile' (cConstantPool cls) x `shouldMatchClass'` cls

    it "can do full read - write - read process" $ do
      let w  = writeClassFile' (cConstantPool cls) x
      let y' = readClassFile w
      y' `shouldSatisfy` isRight
      let Right y = y'
      x `shouldMatchClass` y

shouldMatchClass :: ClassFile High -> ClassFile High -> IO ()
shouldMatchClass y x = do
  cAccessFlags' y `shouldBe` cAccessFlags' x
  cThisClassIndex y `shouldBe` cThisClassIndex x
  cSuperClassIndex y `shouldBe` cSuperClassIndex x
  cInterfaceIndicies' y `shouldBe` cInterfaceIndicies' x
  cFields' y `shouldBe` cFields' x
  forM_ (zip (cMethods y) (cMethods x)) $ \ (ym, xm) -> do
    ym `shouldMatchMethod` xm
  y `shouldBe` x

shouldMatchClass' :: ClassFile Low -> ClassFile Low -> IO ()
shouldMatchClass' y x = do
  cAccessFlags' y `shouldBe` cAccessFlags' x
  cThisClassIndex y `shouldBe` cThisClassIndex x
  cSuperClassIndex y `shouldBe` cSuperClassIndex x
  cInterfaceIndicies' y `shouldBe` cInterfaceIndicies' x
  cFields' y `shouldBe` cFields' x
  forM_ (zip (cMethods y) (cMethods x)) $ \(ym, xm) -> do
    shouldMatchMethod' ym xm

shouldMatchMethod :: Method High -> Method High -> IO ()
shouldMatchMethod ym xm = do
  mAccessFlags ym `shouldBe` mAccessFlags xm
  mName ym `shouldBe` mName xm
  mDescriptor ym `shouldBe` mDescriptor xm
  mExceptions ym `shouldMatchList` mExceptions xm
  case (mCode ym, mCode xm) of
    (Just yc, Just xc) -> do
      cmpOver C.codeByteCodeOprs yc xc $ \ yb xb -> do
        yb `shouldBe` xb
    _ -> mCode ym `shouldBe` mCode xm

shouldMatchMethod' :: Method Low -> Method Low -> IO ()
shouldMatchMethod' ym xm = do
  mAccessFlags ym `shouldBe` mAccessFlags xm
  mNameIndex ym `shouldBe` mNameIndex xm
  mDescriptorIndex ym `shouldBe` mDescriptorIndex xm
  forM_ (zip (unSizedList $ mAttributes ym ) (unSizedList $ mAttributes xm)) $ \(ya, xa) -> do
    case (fromAttribute' ya, fromAttribute' xa) of
      (Right yc, Right xc) -> do
        cmpOn C.codeMaxStack yc xc
        cmpOn C.codeMaxLocals yc xc
        cmpOn C.codeByteCodeInsts yc xc
        cmpOn C.codeExceptionTable yc xc
        cmpOver (List.sort . unSizedList .C.codeAttributes) yc xc $ \ yca xca -> do
          case (fromAttribute' yca, fromAttribute' xca) of
            (Right yst, Right xst) -> do
              cmpOver stackMapTable yst xst $ shouldBe
            (yst, xst) ->
              yst `shouldBe` xst
      (yc, xc) ->
        yc `shouldBe` xc

cmpOn :: (Show b, Eq b) => (a -> b) -> a -> a -> IO ()
cmpOn f a b =
  f a `shouldBe` f b

cmpOver :: (Foldable t) => (a -> t b) -> a -> a -> (b -> b -> IO ()) -> IO ()
cmpOver g ta tb f =
  forM_ (zip (toList . g $ ta) (toList . g $ tb)) (uncurry f)

cmpPrefixes :: (Foldable t) => (a -> t b) -> a -> a -> ([b] -> [b] -> IO ()) -> IO ()
cmpPrefixes g ta tb f =
  forM_ (zip (inits . toList . g $ ta) (inits . toList . g $ tb)) (uncurry f)
