{-|
Module      : Language.JVM.Attribute.StackMapTable
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the StackMapTable Attribute, as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.4).

-}
{-# LANGUAGE DeriveGeneric   #-}

module Language.JVM.Attribute.StackMapTable
  ( Exceptions (..)
  ) where

import           GHC.Generics          (Generic)

import           Data.Binary

import           Language.JVM.Constant (ConstantRef (..))
import           Language.JVM.Utils

-- | An Exceptions attribute is a list of references into the
-- constant pool.
data StackMapTable = StackMapTable
  { stackMapTable :: SizedList16 StackMapFrame
  } deriving (Show, Eq, Generic)

instance Binary StackMapTable where

type DeltaOffset = Word8

data StackMapFrame = StackMapFrame
  { deltaOffset :: DeltaOffset
  , frameType :: StackMapFrameType
  } deriving (Show, Eq)

data StackMapFrameType
  = SameFrame
  | SameLocals1StackItemFrame VerificationTypeInfo
  | SameLocals1StackItemFrameExtended VerificationTypeInfo
  | ChopFrame Int
  | SameFrameExtended
  | AppendFrame [VerificationTypeInfo]
  | FullFrame
      (SizedList16 VerificationTypeInfo)
      (SizedList16 VerificationTypeInfo)
  deriving (Show, Eq)

data VerificationTypeInfo = VerificationTypeInfo
