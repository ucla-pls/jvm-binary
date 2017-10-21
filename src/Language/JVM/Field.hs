{-|
Module      : Language.JVM.Field
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}

{-# LANGUAGE DeriveGeneric #-}
module Language.JVM.Field
  ( Field (..)
  , fName
  , fDescriptor
  , fAccessFlags
  -- * Attributes
  , fConstantValue
  ) where

import           Data.Binary
import           GHC.Generics            (Generic)

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Constant
import           Language.JVM.Utils

import qualified Data.Set as Set

import qualified Data.Text as Text

-- | A Field in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5).
data Field = Field
  { fAccessFlags'    :: BitSet16 FAccessFlag
  , fNameIndex       :: Index Text.Text
  , fDescriptorIndex :: Index FieldDescriptor
  , fAttributes      :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary Field where

-- | Get the name of the field
fName :: Field -> PoolAccess Text.Text
fName = derefF fNameIndex

-- | Get the set of access flags
fAccessFlags :: Field -> Set.Set FAccessFlag
fAccessFlags = toSet . fAccessFlags'

-- | Get the descriptor of the field
fDescriptor :: Field -> PoolAccess FieldDescriptor
fDescriptor = derefF fDescriptorIndex

-- | Fetch the 'ConstantValue' attribute.
-- There can only one be one exceptions attribute on a field.
fConstantValue :: Field -> PoolAccess (Maybe (Either String ConstantValue))
fConstantValue =
  fmap firstOne . matching fAttributes
