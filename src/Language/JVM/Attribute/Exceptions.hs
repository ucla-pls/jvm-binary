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

import           Language.JVM.Constant (Index, ClassName)
import           Language.JVM.Utils

-- | An Exceptions attribute is a list of references into the
-- constant pool.
data Exceptions = Exceptions
  { exceptionIndexTable' :: SizedList16 (Index ClassName)
  } deriving (Show, Eq, Generic, NFData)

-- | Get the constant refs that points .
exceptionIndexTable :: Exceptions -> [(Index ClassName)]
exceptionIndexTable = unSizedList . exceptionIndexTable'

instance Binary Exceptions where
