{-|
Module      : Language.JVM.Attribute.Base
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.JVM.Attribute.Base
  ( Attribute
  , aInfo
  , aName
  ) where

import           Data.Binary
import           GHC.Generics                            (Generic)
import           Control.DeepSeq  (NFData)
import           Data.Text                               as Text

import qualified Data.ByteString                         as BS

import           Language.JVM.Constant
import           Language.JVM.Utils                      (SizedByteString32, unSizedByteString)

-- | An Attribute, simply contains of a reference to a name and
-- contains info.
data Attribute = Attribute
  { aNameIndex :: Index Text.Text
  , aInfo'     :: ! SizedByteString32
  } deriving (Show, Eq, Generic, NFData)

instance Binary Attribute where

-- | A small helper function to extract the info as a
-- lazy 'Data.ByteString.Lazy.ByteString'.
aInfo :: Attribute -> BS.ByteString
aInfo = unSizedByteString . aInfo'

-- | Extracts the name from the attribute, if it exists in the
-- ConstantPool.
aName :: Attribute -> PoolAccess Text.Text
aName = derefF aNameIndex
