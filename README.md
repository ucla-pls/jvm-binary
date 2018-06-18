# jvm-binary

A library for reading and writing Java class-files. To get started
importing the `Language.JVM` file should be sufficient for most uses.

If you want to access Code elements of methods it is recommended to 
import `Language.JVM.Attribute.Code` qualified, like this:

```haskell
import           Language.JVM
import qualified Language.JVM.Attribute.Code as Code

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = 
  ecfl <- readClassFile <$> BL.readFile "test/data/project/Main.class" 
  case ecfl of 
    Right clf -> do
      print (cThisClass clf)
      print (cSuperClass clf)
    Left msg -> 
      print msg
```

## Stages

There are two stages in the current state of the repository. `Low` is closer
to the class-file, while the `High` stage is easier to work with. The reason
that we have the two stages is that the class-file representation has indices
into the Constant Pool. The `High` stage eliminates all these problems.


## Todo's

- Add more Attributes as to the
[docs](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7).
- Add documentation for Code
- Setup regular benchmarks

## Developing

Use stack to build, test, and benchmark.

