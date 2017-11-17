-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import Language.JVM

import Data.ByteString.Lazy as BL

main :: IO ()
main = defaultMain
  [ decodebm
  ]

decodebm :: Benchmark
decodebm = bgroup "decode"
  [ bench "lazy"
      (whnfIO $ decodeClassFile <$> BL.readFile ex2)
  , bench "strict"
      (nfIO $ decodeClassFile <$> BL.readFile ex2)
  , env (do Right clf <- decodeClassFile <$> BL.readFile ex2; return clf) $ \ clf ->
      bgroup "attributes"
      [ bench "code" $ nf allCode clf
      , bench "exception" $ nf allExceptions clf
      , bench "constant" $ nf allConstants clf
      ]
  ]

  where
    allCode :: ClassFile -> Either PoolAccessError [Maybe (Either String Code)]
    allCode = fetchAll cMethods mCode

    allExceptions :: ClassFile -> Either PoolAccessError [Maybe (Either String Exceptions)]
    allExceptions = fetchAll cMethods mExceptions

    allConstants :: ClassFile -> Either PoolAccessError [Maybe (Either String ConstantValue)]
    allConstants = fetchAll cFields fConstantValue

    fetchAll fin fget clf =
      runWithPool (sequence $ fget <$> fin clf)
        $ cConstantPool clf


ex1 :: String
ex1 = "test-suite/data/project/Main.class"

ex2 :: String
ex2 = "test-suite/data/java/util/zip/ZipOutputStream.class"
