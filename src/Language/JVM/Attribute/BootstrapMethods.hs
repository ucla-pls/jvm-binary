{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
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
  ) where

import           Language.JVM.Constant
import           Language.JVM.Attribute.Base
import           Language.JVM.Staged
import           Language.JVM.Utils

-- | 'BootstrapMethods' is an Attribute.
instance IsAttribute (BootstrapMethods Low) where
  attrName = Const "BootstrapMethods"

-- | Is a list of bootstrapped methods.
newtype BootstrapMethods r = BootstrapMethods
  { methods' :: SizedList16 (BootstrapMethod r)
  }

-- | The methods as list
methods :: BootstrapMethods r -> [ BootstrapMethod r ]
methods = unSizedList . methods'

-- | A bootstraped methods.
data BootstrapMethod r = BootstrapMethod
  { method :: !(DeepRef MethodHandle r)
  , arguments :: !(SizedList16 (DeepRef Constant r))
  }

instance Staged BootstrapMethods where
  stage f (BootstrapMethods m) =
    label "BootstrapMethods" $ BootstrapMethods <$> mapM f m

instance Staged BootstrapMethod where
  evolve (BootstrapMethod a m) =
    BootstrapMethod <$> link a <*> mapM link m

  devolve (BootstrapMethod a m) =
    BootstrapMethod <$> unlink a <*> mapM unlink m

$(deriveBaseWithBinary ''BootstrapMethod)
$(deriveBaseWithBinary ''BootstrapMethods)
