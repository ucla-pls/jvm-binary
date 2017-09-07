{-# LANGUAGE DeriveGeneric #-}
module Language.JVM.Field
  ( Field (..)
  , AccessFlags (..)
  , AccessFlag (..)
  ) where

import GHC.Generics (Generic)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Data.Bits
import           Data.List                     (foldl')
import qualified Data.Set                      as S

import           Language.JVM.Attribute (Attribute)
import           Language.JVM.Constant  (ConstantRef)
import           Language.JVM.Utils

data Field = Field
  { accessFlags     :: AccessFlags
  , nameIndex       :: ConstantRef
  , descriptorIndex :: ConstantRef
  , attributes      :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary Field where

data AccessFlag
  = Public
  | Private
  | Protected
  | Static
  | Final
  | Synchronized
  | Unused6
  | Volatile
  | Transient
  | Unused9
  | Unused10
  | Unused11
  | Unused12
  | Synthetic
  | Unused14
  | Enum
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
