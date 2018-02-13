{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Language.JVM.Attribute.BootstrapMethods
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the BootstrapMethods Attribute, as documented
[here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.23).
-}

module Language.JVM.Attribute.BootstrapMethods
  ( BootstrapMethods (..)
  , methods
  , BootstrapMethod (..)
  , arguments
  ) where

import           GHC.Generics          (Generic)

import           Data.Binary
import           Control.DeepSeq

import           Language.JVM.Constant (AbsMethodId, Reference, Ref, Index, Constant)
import           Language.JVM.Utils

-- | Is a list of bootstrapped methods.
data BootstrapMethods r = BootstrapMethods
  { methods' :: SizedList16 (BootstrapMethod r)
  }

instance Binary (BootstrapMethods Index) where

deriving instance Reference r => Show (BootstrapMethods r)
deriving instance Reference r => Eq (BootstrapMethods r)
deriving instance Reference r => Generic (BootstrapMethods r)
deriving instance Reference r => NFData (BootstrapMethods r)

-- | The methods as list
methods :: BootstrapMethods r -> [ BootstrapMethod r ]
methods = unSizedList . methods'

-- | A bootstraped methods.
data BootstrapMethod r = BootstrapMethod
  { methodIndex :: Ref r (AbsMethodId r)
  , arguments' :: SizedList16 (Ref r (Constant r))
  }

-- | The arguments as a list
arguments :: Reference r => BootstrapMethod r -> [ Ref r (Constant r) ]
arguments = unSizedList . arguments'

instance Binary (BootstrapMethod Index) where

deriving instance Reference r => Show (BootstrapMethod r)
deriving instance Reference r => Eq (BootstrapMethod r)
deriving instance Reference r => Generic (BootstrapMethod r)
deriving instance Reference r => NFData (BootstrapMethod r)
