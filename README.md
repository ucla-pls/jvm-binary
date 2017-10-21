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

- Add more Attributes as to the
[docs](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7).
The most notable and required are:

  - LineNumberTable

- Add lenses for better access of deep fields
- Add documentation for Code
- Add Code writing
- Change Megaparsec to Attoparsec of improved performance.
- Setup regular benchmarks

## Developing

Use stack to build, test, and benchmark.

