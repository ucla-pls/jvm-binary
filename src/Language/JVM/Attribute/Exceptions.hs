{-|
Module      : Language.JVM.Attribute.Exceptions
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the Exceptions Attribute, as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.5). It describes the checked
exceptions that a method can make.
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Language.JVM.Attribute.Exceptions
  ( Exceptions (..)
  , exceptionIndexTable
  ) where

import           GHC.Generics          (Generic)
import           Control.DeepSeq       (NFData)

import           Data.Binary

import           Language.JVM.Constant (Index, Ref, Reference, ClassName)
import           Language.JVM.Utils

-- | An Exceptions attribute is a list of references into the
-- constant pool.
data Exceptions r = Exceptions
  { exceptionIndexTable' :: SizedList16 (Ref r ClassName)
  }

deriving instance Reference r => Show (Exceptions r)
deriving instance Reference r => Eq (Exceptions r)
deriving instance Reference r => Generic (Exceptions r)
deriving instance Reference r => NFData (Exceptions r)

-- | Get the constant refs that points .
exceptionIndexTable :: Reference r => Exceptions -> [(Ref r ClassName)]
exceptionIndexTable = unSizedList . exceptionIndexTable'

instance Binary (Exceptions Index) where

deriving instance Reference r => Show (Exception r)
deriving instance Reference r => Eq (Exception r)
deriving instance Reference r => Generic (Exception r)
deriving instance Reference r => NFData (Exception r)
