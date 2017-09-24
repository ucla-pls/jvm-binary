# jvm-binary

A library for reading and writing Java class-files. To get started
importing the `Language.JVM` file should be sufficient for most uses.

If you want to access Code elements of methods it is recommended to 
import `Language.JVM.Attribute.Code` qualified, like this:

```haskell
import           Language.JVM
import qualified Language.JVM.Attribute.Code as Code
```

## Todo's

* The Wide is not yet implemented in the Code attribute.

## Developing

Use stack to build, test, and benchmark.


