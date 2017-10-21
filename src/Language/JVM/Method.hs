{-|
Module      : Language.JVM.Method
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}

{-# LANGUAGE DeriveGeneric   #-}
module Language.JVM.Method
  ( Method (..)

  , mAccessFlags
  , mAttributes

  , mName
  , mDescriptor

  -- * Attributes
  , mCode
  , mExceptions

  ) where

import           Data.Binary
import           Data.Set (Set)
import qualified Data.Text as Text
import           GHC.Generics            (Generic)

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Constant
import           Language.JVM.Utils

-- | A Method in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6).
data Method = Method
  { mAccessFlags'    :: BitSet16 MAccessFlag
  , mNameIndex       :: Index Text.Text
  , mDescriptorIndex :: Index Text.Text
  , mAttributes'     :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary Method where

-- | Unpack the BitSet and get the AccessFlags as a Set.
mAccessFlags :: Method -> Set MAccessFlag
mAccessFlags = toSet . mAccessFlags'

-- | Unpack the SizedList and get the attributes as a List.
mAttributes :: Method -> [Attribute]
mAttributes = unSizedList . mAttributes'

-- | Lookup the name of the method in the 'ConstantPool'.
mName :: Method -> PoolAccess Text.Text
mName = derefF  mNameIndex

-- | Lookup the descriptor of the method in the 'ConstantPool'.
mDescriptor :: Method -> PoolAccess Text.Text
mDescriptor = derefF mDescriptorIndex

-- | Fetch the 'Code' attribute, if any.
-- There can only be one code attribute in a method.
mCode :: Method -> PoolAccess (Maybe (Either String Code))
mCode = fmap firstOne . matching mAttributes

-- | Fetch the 'Exceptions' attribute.
-- There can only be one exceptions attribute in a method.
mExceptions :: Method -> PoolAccess (Maybe (Either String Exceptions))
mExceptions = fmap firstOne . matching mAttributes
