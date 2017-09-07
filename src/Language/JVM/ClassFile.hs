{-# LANGUAGE DeriveGeneric #-}

module Language.JVM.ClassFile
  ( ClassFile (..)
  , AccessFlags (..)
  , AccessFlag (..)

  , decodeClassFile
  , decodeClassFileOrFail
  ) where


import           GHC.Generics (Generic)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.List                     (foldl')
import qualified Data.Set                      as S

import qualified Data.ByteString.Lazy          as BL

import           Language.JVM.Attribute (Attribute)
import           Language.JVM.Constant  (ConstantPool, ConstantRef)
import           Language.JVM.Field     (Field)
import           Language.JVM.Utils
import           Language.JVM.Method    (Method)

data ClassFile = ClassFile
  { magicNumber  :: !Word32

  , minorVersion :: !Word16
  , majorVersion :: !Word16

  , constantPool :: !ConstantPool

  , accessFlags  :: !AccessFlags

  , thisClass    :: !ConstantRef
  , superClass   :: !ConstantRef

  , interfaces   :: SizedList16 ConstantRef
  , fields       :: SizedList16 Field
  , methods      :: SizedList16 Method
  , attributes   :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary ClassFile where

decodeClassFile :: BL.ByteString -> ClassFile
decodeClassFile = decode

decodeClassFileOrFail :: BL.ByteString -> Either String ClassFile
decodeClassFileOrFail bs = do
  case decodeOrFail bs of
    Right (_, _, cf) -> Right cf
    Left (_, _, msg) -> Left msg

data AccessFlag
  = Public
  | Unused1
  | Unused2
  | Unused3
  | Final
  | Super
  | Unused6
  | Unused7
  | Unused8
  | Unused9
  | Abstract
  | Unused11
  | Synthetic
  | Annotation
  | Enum
  | Unused15
  deriving (Ord, Show, Eq, Enum)


newtype AccessFlags = AccessFlags (S.Set AccessFlag)
  deriving (Ord, Show, Eq)

instance Binary AccessFlags where
  get = do
    word <- getWord16be
    return . AccessFlags $ S.fromList [ toEnum x | x <- [0..15], testBit word x ]

  put (AccessFlags f) = do
    let word = foldl' setBit zeroBits (map fromEnum $ S.toList f)
    putWord16be word
