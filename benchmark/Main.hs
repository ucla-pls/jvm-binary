-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main


import Language.JVM.ClassFile (decodeClassFileOrFail)

import Data.ByteString.Lazy as BL

main :: IO ()
main = defaultMain [
  bench "simple load" (whnfIO $ decodeClassFileOrFail <$> BL.readFile "test-suite/project/Main.class")
  ]
