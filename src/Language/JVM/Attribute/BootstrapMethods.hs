{-|
Module      : Language.JVM.Attribute.BootstrapMethods
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the BootstrapMethods Attribute, as documented
[here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.23).
-}
{-# LANGUAGE DeriveGeneric   #-}

module Language.JVM.Attribute.BootstrapMethods
  ( BootstrapMethods (..)
  , methods
  , BootstrapMethod (..)
  , arguments
  ) where

import           GHC.Generics          (Generic)

import           Data.Binary

import           Language.JVM.Constant (ConstantRef (..))
import           Language.JVM.Utils

-- | Is a list of bootstrapped methods.
data BootstrapMethods = BootstrapMethods
  { methods' :: SizedList16 BootstrapMethod
  } deriving (Show, Eq, Generic)

instance Binary BootstrapMethods where

-- | The methods as list
methods :: BootstrapMethods -> [ BootstrapMethod ]
methods = unSizedList . methods'

-- | A bootstraped methods.
data BootstrapMethod = BootstrapMethod
  { methodIndex :: ConstantRef
  , arguments' :: SizedList16 ConstantRef
  } deriving (Show, Eq, Generic)

-- | The arguments is a cool
arguments :: BootstrapMethod -> [ ConstantRef ]
arguments = unSizedList . arguments'

instance Binary BootstrapMethod where
