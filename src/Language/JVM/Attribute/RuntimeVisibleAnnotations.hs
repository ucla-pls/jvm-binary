{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Language.JVM.Attribute.RuntimeVisibleAnnotations
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the RuntimeVisibleAnnotations Attribute, as documented
[here](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.16).
-}
module Language.JVM.Attribute.RuntimeVisibleAnnotations
  ( RuntimeVisibleAnnotations (..)
  ) where

-- text
import qualified Data.Text as Text

-- jvm-binary
import           Language.JVM.Attribute.Base
import           Language.JVM.Constant
import           Language.JVM.Staged

-- -- | 'EnclosingMethod' is an Attribute.
-- instance IsAttribute (RuntimeVisibleAnnotations Low) where
--   attrName = Const "RuntimeVisibleAnnotations"


-- newtype RuntimeVisibleAnnotations r = RuntimeVisibleAnnotations
--   { asListOfRuntimeVisibleAnnotations :: SizedList16 (RuntimeVisibleAnnotation r) }

-- data RuntimeVisibleAnnotation r = RuntimeVisibleAnnotation
--   { type_ :: !(Ref Text.Text r)
--   , values :: !(SizedList16 (ValuePair r))
--   }

-- -- $(deriveBaseWithBinary ''RuntimeVisibleAnnotations)
-- -- $(deriveBaseWithBinary ''RuntimeVisibleAnnotation)
