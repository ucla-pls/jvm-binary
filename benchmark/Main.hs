-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import Language.JVM

import Data.ByteString.Lazy as BL

main :: IO ()
main = defaultMain $
  [ decodebm
  , encodebm
  , evolvebm
  , devolvebm
  ] <*> [ ex2 ]

decodebm :: FilePath -> Benchmark
decodebm fp = bgroup ("decode " ++ fp)
  [ bench "lazy"
      (whnfIO $ decodeClassFile <$> BL.readFile ex2)
  , bench "strict"
      (nfIO $ decodeClassFile <$> BL.readFile ex2)
  ]

encodebm :: FilePath -> Benchmark
encodebm fp =
  env (decodeClassFile' fp) $ \clf ->
    bgroup ("encode " ++ fp)
    [ bench "lazy"
        (whnf encodeClassFile clf)
    , bench "strict"
        (nf encodeClassFile clf)
    ]

evolvebm :: FilePath -> Benchmark
evolvebm fp =
  env (decodeClassFile' fp)$ \clf ->
    bgroup ("evolve " ++ fp)
    [ bench "strict"
        (nf evolveClassFile clf)
    ]

devolvebm :: FilePath -> Benchmark
devolvebm fp =
  envPerBatch (readClassFile' fp) $ \clf ->
    bgroup ("devolve " ++ fp)
    [ bench "strict"
        (nf devolveClassFile clf)
    ]

decodeClassFile' :: FilePath -> IO (ClassFile Low)
decodeClassFile' fp = do
  Right clf <- decodeClassFile <$> BL.readFile fp
  return clf

readClassFile' :: FilePath -> IO (ClassFile High)
readClassFile' fp = do
  Right clf <- readClassFile <$> BL.readFile fp
  return clf

ex1 :: String
ex1 = "test/data/project/Main.class"

ex2 :: String
ex2 = "test/data/java/util/zip/ZipOutputStream.class"
