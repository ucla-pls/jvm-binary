{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Language.JVM.Attribute.MethodParameters
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the MethodParameters Attribute, as documented
[here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.24).
-}
module Language.JVM.Attribute.MethodParameters (
  MethodParameters (..),
  MethodParameter (..),
)
where

import qualified Data.Text as Text

import Language.JVM.AccessFlag
import Language.JVM.Attribute.Base
import Language.JVM.Constant
import Language.JVM.Staged
import Language.JVM.Utils

-- | Is a list of method parameters, one for each parameter
newtype MethodParameters r = MethodParameters
  { methodParameters :: SizedList8 (MethodParameter r)
  }

-- | A method parameter
data MethodParameter r = MethodParameter
  { parameterName :: !(OptionalRef Text.Text r)
  , parameterAccessFlags :: !(BitSet16 PAccessFlag)
  }

$(deriveAll [([''MethodParameter, ''MethodParameters], bases ++ [binary])])

instance Staged MethodParameters where
  stage f (MethodParameters m) =
    label "MethodParameters" $ MethodParameters <$> mapM f m

instance Staged MethodParameter where
  evolve (MethodParameter a m) = MethodParameter <$> optionalLink a <*> pure m

  devolve (MethodParameter a m) = MethodParameter <$> optionalUnlink a <*> pure m

-- | 'BootstrapMethods' is an Attribute.
instance IsAttribute (MethodParameters Low) where
  attrName = Const "MethodParameters"
