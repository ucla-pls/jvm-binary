{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Language.JVM.Attribute.ConstantValue
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the ConstantValue, as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.5).
-}

module Language.JVM.Attribute.ConstantValue
  ( ConstantValue (..)
  ) where

import           Control.DeepSeq       (NFData)
import           GHC.Generics          (Generic)

import           Data.Binary

import           Language.JVM.Constant (Constant, Index, Ref, Reference)

-- | A constant value is just a index into the constant pool.
data ConstantValue r = ConstantValue
  { constantValueIndex :: Ref r (Constant r)
  }

deriving instance Reference r => Show (ConstantValue r)
deriving instance Reference r => Eq (ConstantValue r)
deriving instance Reference r => Generic (ConstantValue r)
deriving instance Reference r => NFData (ConstantValue r)

instance Binary (ConstantValue Index) where
