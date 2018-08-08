{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module      : Language.JVM.Attribute.EnclosingMethod
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the EnclosingMethod Attribute,
as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.7).
-}

module Language.JVM.Attribute.EnclosingMethod
  ( EnclosingMethod (..)
  ) where

import           Language.JVM.Attribute.Base
import           Language.JVM.Constant
import           Language.JVM.Staged

-- | 'EnclosingMethod' is an Attribute.
instance IsAttribute (EnclosingMethod Low) where
  attrName = Const "EnclosingMethod"

-- | The 'EnclosingMethod' is a reference to the enclosing method of the class
data EnclosingMethod r = EnclosingMethod
  { enclosingClassName :: !(Ref ClassName r)
  , enclosingMethodName :: !(Ref (Maybe MethodId) r)
  }


instance Staged EnclosingMethod where
  evolve (EnclosingMethod cn mn) = label "EnclosingMethod" $ do
    EnclosingMethod
      <$> link cn
      <*> if mn == 0 then return Nothing else Just <$> link mn

  devolve (EnclosingMethod cn mn) = label "EnclosingMethod" $ do
    EnclosingMethod
      <$> unlink cn
      <*> case mn of
            Nothing -> return 0
            Just mn' -> unlink mn'

$(deriveBaseWithBinary ''EnclosingMethod)
