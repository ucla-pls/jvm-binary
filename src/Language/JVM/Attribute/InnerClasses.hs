{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Language.JVM.Attribute.InnerClasses
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the InnerClasses Attribute,
as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.6).
-}
module Language.JVM.Attribute.InnerClasses (
  InnerClasses (..),
  InnerClass (..),
) where

import qualified Data.Text as Text

import Language.JVM.AccessFlag
import Language.JVM.Attribute.Base
import Language.JVM.Constant
import Language.JVM.Staged
import Language.JVM.Utils

-- | The 'InnerClasses' is a reference to the enclosing method of the class
newtype InnerClasses r = InnerClasses
  { innerClasses :: Choice (SizedList16 (InnerClass Low)) [InnerClass High] r
  }

data InnerClass r = InnerClass
  { icClassName :: !(Ref ClassName r)
  , icOuterClassName :: !(Ref (Maybe ClassName) r)
  , icInnerName :: !(Ref (Maybe Text.Text) r)
  , icInnerAccessFlags :: !(BitSet16 ICAccessFlag)
  }

instance Staged InnerClasses where
  evolve (InnerClasses (SizedList r)) = InnerClasses <$> mapM evolve r
  devolve (InnerClasses r) = InnerClasses . SizedList <$> mapM devolve r

instance Staged InnerClass where
  evolve (InnerClass cn ocn inn iac) = label "InnerClass" $ do
    InnerClass
      <$> link cn
      <*> (if ocn == 0 then return Nothing else Just <$> link ocn)
      <*> (if inn == 0 then return Nothing else Just <$> link inn)
      <*> pure iac

  devolve (InnerClass cn mn inn iac) = label "InnerClass" $ do
    InnerClass
      <$> unlink cn
      <*> case mn of
        Nothing -> return 0
        Just mn' -> unlink mn'
      <*> case inn of
        Nothing -> return 0
        Just inn' -> unlink inn'
      <*> pure iac

$(deriveAll [([''InnerClasses, ''InnerClass], bases ++ [binary])])

-- | 'InnerClasses' is an Attribute.
instance IsAttribute (InnerClasses Low) where
  attrName = Const "InnerClasses"
