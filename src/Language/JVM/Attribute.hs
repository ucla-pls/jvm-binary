{-# LANGUAGE DeriveGeneric #-}
module Language.JVM.Attribute
  ( Attribute (..)
  , info'

  -- SubAttributes
--  , module Language.JVM.Attribute.Code
  , toCode
  ) where

import           GHC.Generics                (Generic)

import           Data.Binary
import           Data.Bifunctor
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL

import           Language.JVM.Constant       (ConstantRef)
import           Language.JVM.Utils          (SizedByteString32 (..), trd)

import qualified Language.JVM.Attribute.Code

data Attribute = Attribute
  { nameIndex :: ! ConstantRef
  , info      :: ! SizedByteString32
  } deriving (Show, Eq, Generic)

instance Binary Attribute where


-- | A small helper function to extract the info as a
-- lazy `ByteString`.
info' :: Attribute -> BS.ByteString
info' = unSizedByteString32 . info

type Code = Language.JVM.Attribute.Code.Code Attribute

toCode :: Attribute -> Either String Code
toCode =
  bimap trd trd . decodeOrFail . BL.fromStrict . info'
