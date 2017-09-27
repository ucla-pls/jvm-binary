{-|
Module      : Language.JVM.Field
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}

{-# LANGUAGE DeriveGeneric #-}
module Language.JVM.Field
  ( Field (..)

  -- * Attributes
  , fConstantValue
  ) where

import           Data.Binary
import           Data.Monoid
import           GHC.Generics            (Generic)

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute  (Attribute, ConstantValue, fromAttribute')
import           Language.JVM.Constant   (ConstantRef, ConstantPool)
import           Language.JVM.Utils

-- | A Field in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5).
data Field = Field
  { fAccessFlags     :: BitSet16 FAccessFlag
  , fNameIndex       :: ! ConstantRef
  , fDescriptorIndex :: ! ConstantRef
  , fAttributes      :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary Field where

-- | Fetch the 'ConstantValue' attribute.
-- There can only one be one exceptions attribute on a field.
fConstantValue :: ConstantPool -> Field -> Maybe (Either String ConstantValue)
fConstantValue cp =
  getFirst . foldMap (First . fromAttribute' cp) . fAttributes
