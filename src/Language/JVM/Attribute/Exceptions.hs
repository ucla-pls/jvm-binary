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
  ) where

import           Language.JVM.Attribute.Base
import           Language.JVM.Staged
import           Language.JVM.Constant
import           Language.JVM.Utils


-- | 'Exceptions' is an Attribute.
instance IsAttribute (Exceptions Low) where
  attrName = Const "Exceptions"

-- | An Exceptions attribute is a list of references into the
-- constant pool.
newtype Exceptions r = Exceptions
  { exceptions :: SizedList16 (Ref ClassName r)
  }

instance Staged Exceptions where
  evolve (Exceptions t) = Exceptions <$> mapM link t
  devolve (Exceptions t) = Exceptions <$> mapM unlink t

$(deriveBaseWithBinary ''Exceptions)
