module Language.JVM
  ( module Language.JVM.Attribute
  , module Language.JVM.ClassFile
  , module Language.JVM.Constant
  , module Language.JVM.Field
  , module Language.JVM.Method

  , decodeClassFile
  , getCodeOfMethod
  , constantOf
  , textOf
  ) where


import qualified Data.ByteString.Lazy   as BL
import           Data.Binary
import qualified Data.Text as Text

import    Data.Monoid

import           Language.JVM.Attribute
import           Language.JVM.ClassFile
import           Language.JVM.Constant
import           Language.JVM.Field
import           Language.JVM.Method

decodeClassFile :: BL.ByteString -> Either String ClassFile
decodeClassFile bs = do
  case decodeOrFail bs of
    Right (_, _, cf) -> Right cf
    Left (_, _, msg) -> Left msg


textOf :: ClassFile -> ConstantRef -> Maybe Text.Text
textOf cf cr =
  lookupText cr (cConstantPool cf)

constantOf :: ClassFile -> ConstantRef -> Maybe Constant
constantOf cf cr =
  lookupConstant cr (cConstantPool cf)

getCodeOfMethod :: ClassFile -> Method -> Maybe (Either String Code)
getCodeOfMethod cf m = do
  getFirst . foldMap (First . fromAttribute' (cConstantPool cf)) $ mAttributes m
