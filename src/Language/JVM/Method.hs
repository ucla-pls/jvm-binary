{-# LANGUAGE DeriveGeneric   #-}
module Language.JVM.Method
  ( Method (..)
  ) where

import           Data.Binary
import           GHC.Generics            (Generic)

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute  (Attribute)
import           Language.JVM.Constant   (ConstantRef)
import           Language.JVM.Utils

data Method = Method
  { mAccessFlags     :: BitSet16 MAccessFlag
  , mNameIndex       :: ! ConstantRef
  , mDescriptorIndex :: ! ConstantRef
  , mAttributes      :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary Method where

