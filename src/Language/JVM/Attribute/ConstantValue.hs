{-|
Module      : Language.JVM.Attribute.ConstantValue
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the ConstantValue, as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.5).
-}
{-# LANGUAGE DeriveGeneric   #-}

module Language.JVM.Attribute.ConstantValue
  ( ConstantValue (..)
  ) where

import           GHC.Generics          (Generic)

import           Data.Binary

import           Language.JVM.Constant (Index, Constant)

-- | A constant value is just a index into the constant pool.
data ConstantValue = ConstantValue
  { constantValueIndex :: Index Constant
  } deriving (Show, Eq, Generic)

instance Binary ConstantValue where
