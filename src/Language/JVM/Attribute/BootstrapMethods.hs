{-# LANGUAGE TemplateHaskell #-}
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
  , method
  , argumentIndicies
  , arguments
  ) where

import           Language.JVM.Constant
import           Language.JVM.Stage
import           Language.JVM.Utils

-- | Is a list of bootstrapped methods.
newtype BootstrapMethods r = BootstrapMethods
  { methods' :: SizedList16 (BootstrapMethod r)
  }

-- | The methods as list
methods :: BootstrapMethods r -> [ BootstrapMethod r ]
methods = unSizedList . methods'

-- | A bootstraped methods.
data BootstrapMethod r = BootstrapMethod
  { methodIndex :: DeepRef (InClass MethodId) r
  , argumentIndicies' :: SizedList16 (DeepRef Constant r)
  }

method :: BootstrapMethod High -> InClass MethodId High
method =
  valueF $ unDeep . methodIndex

-- | The arguments as a list
argumentIndicies :: BootstrapMethod r -> [ DeepRef Constant r ]
argumentIndicies = unSizedList . argumentIndicies'

-- | The arguments as a list
arguments :: BootstrapMethod High -> [ Constant High ]
arguments = map (value . unDeep) . argumentIndicies

instance Staged BootstrapMethods where
  stage f (BootstrapMethods m) =
    BootstrapMethods <$> mapM f m

instance Staged BootstrapMethod where
  stage f (BootstrapMethod a m) =
    BootstrapMethod <$> f a <*> mapM f m

$(deriveBaseWithBinary ''BootstrapMethod)
$(deriveBaseWithBinary ''BootstrapMethods)
