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
  , mCode
  ) where

import           Data.Binary
import           Data.Set (Set)
import qualified Data.Text as Text
import           GHC.Generics            (Generic)

import           Data.Monoid

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute  (Attribute, fromAttribute', Code)
import           Language.JVM.Constant   (ConstantRef, ConstantPool, lookupText)
import           Language.JVM.Utils

-- | A Method in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6).
data Method = Method
  { mAccessFlags'    :: BitSet16 MAccessFlag
  , mNameIndex       :: ! ConstantRef
  , mDescriptorIndex :: ! ConstantRef
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
mName :: ConstantPool -> Method -> Maybe Text.Text
mName cp = flip lookupText cp . mNameIndex

-- | Lookup the descriptor of the method in the 'ConstantPool'.
mDescriptor :: ConstantPool -> Method -> Maybe Text.Text
mDescriptor cp = flip lookupText cp . mDescriptorIndex

-- | Fetch the first 'Code' Attribute.
mCode :: ConstantPool -> Method -> Maybe (Either String Code)
mCode cp =
  getFirst . foldMap (First . fromAttribute' cp) . mAttributes
