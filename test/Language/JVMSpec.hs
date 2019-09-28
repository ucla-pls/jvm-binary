{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Language.JVMSpec where

import           SpecHelper

import Test.Hspec hiding (shouldBe, shouldSatisfy, shouldMatchList)

import qualified Data.ByteString.Lazy                 as BL
import           Data.Either
import           Data.Foldable
import           Data.List                            as List
import qualified Data.Text                            as Text

-- prelude
import System.Environment

-- filepath
import System.FilePath

-- vector
import qualified Data.Vector                          as V

-- zip-archive
import           Codec.Archive.Zip

import           Language.JVM
import qualified Language.JVM.Attribute.Code          as C
import           Language.JVM.Attribute.StackMapTable


spec :: Spec
spec = do
  describe "Main.class" $ do
    fl <- runIO $ BL.readFile "test/data/project/Main.class"

    it "can read classfile from file" $ do
      case readClassFile fl of
        Right clf -> do
          cThisClass clf `shouldBe` "Main"
          cSuperClass clf `shouldBe` "java/lang/Object"
        Left msg ->
          fail $ show msg
    it "can decode a classfile from file" $ do
      case decodeClassFile fl of
        Right _ ->
          True `shouldBe` True
        Left msg ->
          fail $ show msg

  spec_reading_classfile

  describe "the standard library" $ do
    runIO (lookupEnv "JAVA_HOME") >>= \case
      Just home -> do
        archive <- runIO $ either (error . ("Could not read zip file: "++) . show) return =<< readZipFile (home </> "jre/lib/rt.jar")

        let priorities =
              [ "java/lang/Class.class"
              ]

        forM_ priorities $ \priority -> do
          let Just entry = findEntryByPath priority archive
          it ("can read priority " ++ priority) $ do
            case readClassFile (fromEntry entry) of
              Right _ ->
                True `shouldBe` True
              Left msg ->
                fail $ show msg

        let classes =
              filter (\entry -> takeExtension (eRelativePath entry) == ".class")
              (zEntries archive)
        forM_ classes $ \entry -> do
          it ("can read " ++ eRelativePath entry) $ do
            case readClassFile (fromEntry entry) of
              Right _ ->
                True `shouldBe` True
              Left msg ->
                fail $ show msg

        forM_ classes $ \entry -> do
          it ("can read " ++ eRelativePath entry) $ do
            case readClassFile (fromEntry entry) of
              Right _ ->
                True `shouldBe` True
              Left msg ->
                fail $ show msg
      Nothing -> do
        runIO $ putStrLn "Expecting JAVA_HOME to be set"


spec_reading_classfile :: Spec
spec_reading_classfile = testAllFiles $ \bs -> do
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
      case (runEvolve (EvolveConfig [] cp' (const True)) (evolve m)) of
        Right _ -> return ()
        Left err -> do
          putStr (show err) >> putStr ": "
          print . runEvolve (EvolveConfig [] cp' (const True)) $ do
            x <- link (mDescriptor m)
            n <- link (mName m)
            return ((n, x) :: (MethodDescriptor, Text.Text))
          forM_ (mAttributes m) $ \a -> do
            -- Assume code
            case fromAttribute' a :: Either String (C.Code Low) of
              Right c -> do
                forM_ (byteCodeInstructions . C.codeByteCode $ c) $ \i ->
                  putStr " -> " >> print i

                forM_ (C.codeAttributes c) $ \ca -> do
                  print $ runEvolve (EvolveConfig [] cp' (const True)) (evolve ca)
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
    let me = evolveClassFile (const True) cls
    it "can evolve the whole class file" $ do
      me `shouldSatisfy` isRight

    let Right x = me
    it "has same or smaller constant pool" $ do
      let d' = devolveClassFile x
      (poolCount $ cConstantPool d') `shouldSatisfy`
        (<= (poolCount $ cConstantPool cls))

    -- it "is the same when devolving with the original constant pool" $
    --   devolveClassFile' (cConstantPool cls) x `shouldMatchClass'` cls

    it "can do full read - write - read process" $ do
      let w  = writeClassFile x
      let y' = readClassFile w
      y' `shouldSatisfy` isRight
      let Right y = y'
      x `shouldMatchClass` y

shouldMatchClass :: ClassFile High -> ClassFile High -> IO ()
shouldMatchClass y x = do
  cAccessFlags' y `shouldBe` cAccessFlags' x
  cThisClass y `shouldBe` cThisClass x
  cSuperClass y `shouldBe` cSuperClass x
  cInterfaces y `shouldBe` cInterfaces x
  cFields' y `shouldBe` cFields' x
  forM_ (zip (cMethods y) (cMethods x)) $ \ (ym, xm) -> do
    ym `shouldMatchMethod` xm
  y `shouldBe` x

shouldMatchClass' :: ClassFile Low -> ClassFile Low -> IO ()
shouldMatchClass' y x = do
  cAccessFlags' y `shouldBe` cAccessFlags' x
  cThisClass y `shouldBe` cThisClass x
  cSuperClass y `shouldBe` cSuperClass x
  cInterfaces y `shouldBe` cInterfaces x
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
  mName ym `shouldBe` mName xm
  mDescriptor ym `shouldBe` mDescriptor xm
  forM_ (zip (unSizedList $ mAttributes ym ) (unSizedList $ mAttributes xm)) $ \(ya, xa) -> do
    case (fromAttribute' ya, fromAttribute' xa) of
      (Right yc, Right xc) -> do
        cmpOn C.codeMaxStack yc xc
        cmpOn C.codeMaxLocals yc xc
        cmpOn (C.codeByteCodeInsts :: Code Low -> V.Vector (ByteCodeInst Low)) yc xc
        cmpOn C.codeExceptionTable yc xc
        cmpOver (List.sort . unSizedList . C.codeAttributes) yc xc $ \ yca xca -> do
          case (fromAttribute' yca, fromAttribute' xca)
             :: (Either String (StackMapTable Low), Either String (StackMapTable Low)) of
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

readZipFile :: FilePath -> IO (Either String Archive)
readZipFile =
  fmap toArchiveOrFail . BL.readFile
