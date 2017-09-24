{-|
Module      : Language.JVM.Attribute.Exceptions
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the Exceptions Attribute, as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.5). It describes the checked
exceptions that a method can make.
-}
{-# LANGUAGE DeriveGeneric   #-}

module Language.JVM.Attribute.Exceptions
  ( Exceptions (..)
  ) where

import           GHC.Generics          (Generic)

import           Data.Binary

import           Language.JVM.Constant (ConstantRef (..))
import           Language.JVM.Utils

-- | An Exceptions attribute is a list of references into the
-- constant pool.
data Exceptions = Exceptions
  { exceptionIndexTable :: SizedList16 ConstantRef
  } deriving (Show, Eq, Generic)

instance Binary Exceptions where
