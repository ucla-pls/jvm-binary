{-|
Module      : Language.JVM.Attribute.ConstantValue
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the ConstantValue, as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.5).
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Language.JVM.Attribute.ConstantValue
  ( ConstantValue (..)
  ) where

import           GHC.Generics          (Generic)
import           Control.DeepSeq       (NFData)

import           Data.Binary

import           Language.JVM.Constant (Index, Ref, Reference,  Constant)

-- | A constant value is just a index into the constant pool.
data ConstantValue r = ConstantValue
  { constantValueIndex :: Ref r Constant
  }

deriving instance Reference r => Show (ConstantValue r)
deriving instance Reference r => Eq (ConstantValue r)
deriving instance Reference r => Generic (ConstantValue r)
deriving instance Reference r => NFData (ConstantValue r)

instance Binary (ConstantValue Index) where
