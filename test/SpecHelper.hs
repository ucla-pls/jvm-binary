module SpecHelper
  ( module Test.Tasty
  , module Test.Tasty.Hspec
  , module Test.Tasty.QuickCheck
  , decode
  , encode
  , blReadFile
  , isoBinary
  , testAllFiles
  , testSomeFiles
  ) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import qualified Data.ByteString.Lazy as BL

import System.FilePath
import System.Directory

import Control.Monad
import Data.Binary
import Data.Bits

blReadFile :: FilePath -> IO BL.ByteString
blReadFile = BL.readFile

toHex :: Word8 -> String
toHex x =
  [ alpha !! fromIntegral (x `shift` (-4))
  , alpha !! fromIntegral (x `mod` 16)
  ]
  where alpha = "0123456789abcdef"

testAllFiles :: SpecWith BL.ByteString -> IO [TestTree]
testAllFiles spec = do
  files <- filter isClass <$> recursiveContents "test/data"
  forM files $ \file -> testSpec file (beforeAll (blReadFile file) spec)
  where
    isClass p = takeExtension p == ".class"


testSomeFiles :: SpecWith BL.ByteString -> IO [TestTree]
testSomeFiles spec =
  forM files $ \file -> testSpec file (beforeAll (blReadFile file) spec)
  where
    files =
      [ "test/data/java/util/zip/ZipOutputStream.class"
      , "test/data/project/Main.class"
      ]

isoBinary :: (Binary a, Eq a, Show a) => a -> Property
isoBinary a =
  let bs = encode a
  in counterexample (concat . map toHex $ BL.unpack bs) $
      decode bs === a

folderContents :: FilePath -> IO [ FilePath ]
folderContents fp =
  map (fp </>) <$> listDirectory fp

recursiveContents :: FilePath -> IO [ FilePath ]
recursiveContents fp = do
  test <- doesDirectoryExist fp
  (fp:) <$> if test then do
    content <- folderContents fp
    concat <$> mapM recursiveContents content
  else return []
