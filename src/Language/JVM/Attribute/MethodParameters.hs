{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Language.JVM.Attribute.MethodParameters
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the MethodParameters Attribute, as documented
[here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.24).
-}

module Language.JVM.Attribute.MethodParameters
  ( MethodParameters(..)
  , MethodParameter(..)
  )
where

import qualified Data.Text                     as Text

import           Language.JVM.Constant
import           Language.JVM.Attribute.Base
import           Language.JVM.Staged
import           Language.JVM.Utils
import           Language.JVM.AccessFlag

-- | 'BootstrapMethods' is an Attribute.
instance IsAttribute (MethodParameters Low) where
  attrName = Const "MethodParameters"

-- | Is a list of method parameters, one for each parameter
newtype MethodParameters r = MethodParameters
  { methodParameters :: SizedList8 (MethodParameters r)
  }

-- | A method parameter
data MethodParameter r = MethodParameter
  { parameterName        :: !(Ref Text.Text r)
  , parameterAccessFlags :: !(BitSet16 PAccessFlag)
  }


instance Staged MethodParameters where
  stage f (MethodParameters m) =
    label "BootstrapMethods" $ MethodParameters <$> mapM f m

instance Staged MethodParameter where
  evolve (MethodParameter a m) = MethodParameter <$> link a <*> pure m

  devolve (MethodParameter a m) = MethodParameter <$> unlink a <*> pure m

$(deriveBaseWithBinary ''MethodParameter)
$(deriveBaseWithBinary ''MethodParameters)
