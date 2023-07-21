{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Language.JVM.Attribute.ConstantValue
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the ConstantValue, as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.5).
-}
module Language.JVM.Attribute.ConstantValue (
  ConstantValue (..),
)
where

import Language.JVM.Attribute.Base
import Language.JVM.Constant
import Language.JVM.Staged

-- | A constant value is just a index into the constant pool.
newtype ConstantValue r = ConstantValue
  { constantValue :: Ref JValue r
  }

$(deriveBaseWithBinary ''ConstantValue)

-- | 'ConstantValue' is an Attribute.
instance IsAttribute (ConstantValue Low) where
  attrName = Const "ConstantValue"

instance Staged ConstantValue where
  evolve (ConstantValue r) = ConstantValue <$> link r
  devolve (ConstantValue r) = ConstantValue <$> unlink r
