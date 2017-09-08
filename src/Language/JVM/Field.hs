{-# LANGUAGE DeriveGeneric #-}
module Language.JVM.Field
  ( Field (..)
  ) where

import           Data.Binary
import           GHC.Generics            (Generic)

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute  (Attribute)
import           Language.JVM.Constant   (ConstantRef)
import           Language.JVM.Utils

data Field = Field
  { fAccessFlags     :: BitSet16 FAccessFlag
  , fNameIndex       :: ! ConstantRef
  , fDescriptorIndex :: ! ConstantRef
  , fAttributes      :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary Field where
