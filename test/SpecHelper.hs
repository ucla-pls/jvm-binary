
module SpecHelper
  ( module Test.Tasty
  , module Test.Hspec.Expectations.Pretty
  , module Test.Tasty.QuickCheck
  , module Generic.Random
  , decode
  , encode
  , blReadFile
  , isoBinary
  , isoRoundtrip
  , isoByteCodeRoundtrip
  , byteCodeRoundtrip
  , testAllFiles
  , hexStringS
  , hexString
  , Spec
  , SpecWith
  , it
  , xit
  , describe
  ) where

import Test.Hspec.Expectations.Pretty

import Test.Tasty
import Test.Tasty.Hspec hiding (shouldBe)
import Test.Tasty.QuickCheck 
import qualified Test.QuickCheck.Property as P
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Data.Bifunctor

import System.FilePath
import System.Directory
import           Generic.Random hiding (Field)

import Control.Monad
import Data.Binary
import Data.Bits
import qualified Data.List as List

import Language.JVM.ByteCode
import Language.JVM.Utils
import Language.JVM.Staged
import Language.JVM.ClassFileReader

blReadFile :: FilePath -> IO BL.ByteString
blReadFile = BL.readFile

toHex :: Word8 -> String
toHex x =
  [ alpha !! fromIntegral (x `shift` (-4))
  , alpha !! fromIntegral (x `mod` 16)
  ]
  where alpha = "0123456789abcdef"

testAllFiles :: (BL.ByteString -> Spec) -> IO [TestTree]
testAllFiles spec = do
  files <- filter isClass <$> recursiveContents "test/data"
  forM files $ \file -> do
    bs <- blReadFile file
    testSpec file $ spec bs
  where
    isClass p =
      takeExtension p == ".class"
      && p /= "test/data/SQLite.class"


-- testSomeFiles :: SpecWith BL.ByteString -> IO [TestTree]
-- testSomeFiles spec =
--   forM files $ \file -> testSpec file (beforeAll (blReadFile file) spec)
--   where
--     files =
--       [ "test/data/java/util/zip/ZipOutputStream.class"
--       , "test/data/project/Main.class"
--       , "test/data/com/sun/istack/internal/localization/Localizable.class"
--       , "test/data/SQLite.class"
--       , "test/data/Emitter.class"
--       , "test/data/EventExecutorGroup.class"
--       , "test/data/NioEventLoopGroup.class"
--       ]

hexStringS :: BS.ByteString -> String
hexStringS =
  hexString . BL.fromStrict

hexString :: BL.ByteString -> String
hexString =
  List.intercalate " " . group 8 . concat . map toHex . BL.unpack

isoBinary :: (Binary a, Eq a, Show a) => a -> Property
isoBinary a =
  let bs = encode a
  in counterexample (hexString bs) $
      decode bs === a

-- | Test that a value can go from the Highest state to binary and back again
-- without losing data.
isoRoundtrip ::
  (Staged a, Eq (a High), Show (a High), Binary (a Low))
  => (a High) -> Property
isoRoundtrip a =
  case roundtrip a of
    Right (_, a') ->
      a' === a
    Left msg -> property $ P.failed { P.reason = msg }
  where
    roundtrip a1 = do
      let (a', CPBuilder _ cp) = runConstantPoolBuilder (devolve a1) cpbEmpty
      let bs = encode a'
      a'' <- bimap trd trd $ decodeOrFail bs
      cp' <- first show $ bootstrapConstantPool cp
      a3 <- first show $ runEvolve cp' (evolve a'')
      return (bs, a3)

-- | Test that a value can go from the Highest state to binary and back again
-- without losing data.
isoByteCodeRoundtrip ::
  (ByteCodeStaged a, Eq (a High), Show (a High), Binary (a Low))
  => (a High) -> Property
isoByteCodeRoundtrip a =
  case byteCodeRoundtrip a of
    Right (_, a') ->
      a' === a
    Left msg -> property $ P.failed { P.reason = msg }
  where

byteCodeRoundtrip :: (ByteCodeStaged s, Binary (s Low)) => s High -> Either String (BL.ByteString, s High)
byteCodeRoundtrip a1 = do
  let (a', CPBuilder _ cp) = runConstantPoolBuilder (devolveBC (return . fromIntegral) a1) cpbEmpty
  let bs = encode a'
  a'' <- bimap trd trd $ decodeOrFail bs
  cp' <- first show $ bootstrapConstantPool cp
  a3 <- first show $ runEvolve cp' (evolveBC (return . fromIntegral) a'')
  return (bs, a3)

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

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"
