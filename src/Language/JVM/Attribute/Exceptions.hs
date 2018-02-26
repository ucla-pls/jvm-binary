{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Language.JVM.Attribute.Base
import           Language.JVM.Staged
import           Language.JVM.Constant
import           Language.JVM.Utils


-- | 'Exceptions' is an Attribute.
instance IsAttribute Exceptions where
  attrName = Const "Exceptions"

-- | An Exceptions attribute is a list of references into the
-- constant pool.
newtype Exceptions r = Exceptions
  { exceptionIndexTable' :: SizedList16 (Ref ClassName r)
  }

-- | Get the constant refs that points .
exceptionIndexTable :: Exceptions r -> [(Ref ClassName r)]
exceptionIndexTable = unSizedList . exceptionIndexTable'

instance Staged Exceptions where
  evolve (Exceptions t) = Exceptions <$> mapM evolve t
  devolve (Exceptions t) = Exceptions <$> mapM devolve t

$(deriveBaseWithBinary ''Exceptions)
