-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import Language.JVM

import Data.ByteString.Lazy as BL

main :: IO ()
main = defaultMain
  [ bench "simple load" (
      whnfIO $ decodeClassFile <$>
        BL.readFile "test-suite/data/project/Main.class"
      )
  ]
