{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-|
Module      : Language.JVM.Attribute.Exceptions
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the Exceptions Attribute, as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.5). It describes the checked
exceptions that a method can make.
-}
module Language.JVM.Attribute.Exceptions
  ( Exceptions (..)
  , exceptionIndexTable
  ) where

import           Language.JVM.Stage
import           Language.JVM.Constant
import           Language.JVM.Utils

-- | An Exceptions attribute is a list of references into the
-- constant pool.
data Exceptions r = Exceptions
  { exceptionIndexTable' :: SizedList16 (Ref ClassName r)
  }

-- | Get the constant refs that points .
exceptionIndexTable :: Exceptions r -> [(Ref ClassName r)]
exceptionIndexTable = unSizedList . exceptionIndexTable'

$(deriveBaseWithBinary ''Exceptions)
