{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- {-# LANGUAGE OverloadedStrings #-}


-- import           Data.Binary                              (decode, encode)
-- import qualified Data.ByteString                          as BS
-- import qualified Data.ByteString.Char8                    as BC
-- import qualified Data.ByteString.Lazy                     as LBS

-- import           Test.Tasty
-- import           Test.Tasty.Hspec
-- import           Test.Tasty.QuickCheck                    as QC

-- import           Language.JVM.Constant

-- import qualified Language.JVM.Attribute.CodeTest          as CodeTest
-- import qualified Language.JVM.Attribute.StackMapTableTest as StackMapTableTest
-- import qualified Language.JVM.AttributeTest               as AttributeTest
-- import qualified Language.JVM.ClassFileTest               as ClassFileTest
-- import qualified Language.JVM.ConstantTest                as ConstantTest
-- import qualified Language.JVM.FieldTest                   as FieldTest
-- import qualified Language.JVM.MethodTest                  as MethodTest
-- import qualified Language.JVM.TypeTest                    as TypeTest
-- import qualified Language.JVM.UtilsTest                   as UtilsTest
-- import qualified Language.JVMTest                         as JVMTest


-- main :: IO ()
-- main = do
--   tg <- testGroup "Tests" <$> sequence
--     [ ConstantTest.tests
--     , JVMTest.tests
--     ]
--   defaultMain tg

-- -- main :: IO ()
-- -- main = do
-- --   putStrLn ".."
-- --   test <- testSpec "jvmhs" classfileSpec
-- --   Test.Tasty.defaultMain (Test.Tasty.testGroup "All of it" [ test, encodedecode ])

-- -- classfileSpec :: Spec
-- -- classfileSpec = beforeAll (LBS.readFile "test-suite/project/Main.class") $ do
-- --   it "can read the bytestring" $
-- --     \bs -> let classfile = decode bs
-- --            in (magicNumber classfile) `shouldBe` 3405691582

-- --   it "can encode and decode attributes" $
-- --     \_ -> let attr = Attribute 1 (BC.pack "Hello")
-- --            in (decode . encode ) attr `shouldBe` attr

-- --   it "will encode and decode back to a class file" $ \bs ->
-- --     let classfile = decode bs :: ClassFile
-- --         encoding = encode classfile
-- --     in (methods classfile) `shouldBe` (methods $ decode (LBS.append encoding (LBS.repeat 0)))

-- --   it "will encode the data back to the same format" $ \bs ->
-- --     let classfile = decode bs :: ClassFile
-- --     in bs `shouldBe` encode classfile


-- -- encodedecode :: Test.Tasty.TestTree
-- -- encodedecode =
-- --   Test.Tasty.testGroup "encode . decode == id"
-- --   [ QC.testProperty "Attribute" $ \(ArbAttribute attr) ->
-- --       (decode . encode) attr == attr
-- --   ]

-- -- newtype ArbAttribute =
-- --   ArbAttribute Attribute deriving (Show, Eq)

-- -- instance Arbitrary ArbAttribute where
-- --   arbitrary = do
-- --     index <- arbitrary
-- --     len <- choose (0, 50)
-- --     bs <- BS.pack <$> sequence (replicate len arbitrary)
-- --     return $ ArbAttribute (Attribute index bs)
