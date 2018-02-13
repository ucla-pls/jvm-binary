
module Language.JVM.ClassFileReader
  ( -- ClassFileReader (..)
 -- ,
  Ref (..)
  , Deref (..)
  ) where

import Language.JVM.Constant

-- | An error while reading a class file is represented using
-- this data structure
data ClassFileError
  = CFEPoolAccessError !PoolAccessError

-- type ClassFileReader f =
--   ConstantPool -> f Ref -> Either ClassFileError (f Deref)

-- class ClassFileReadable f where
--   read :: ClassFileReader f
