{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module      : Language.JVM.Method
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.JVM.Method
  ( Method (..)
  , mAccessFlags

  -- * Attributes
  , MethodAttributes (..)
  , mCode
  , mExceptions'
  , mExceptions
  , mSignature

  ) where

import           Data.Maybe
import           Data.Monoid
import           Data.Set                          (Set)
import qualified Data.Text                         as Text

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Attribute.Exceptions (exceptions)
import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.Type
import           Language.JVM.Utils

-- | A Method in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6).
data Method r = Method
  { mAccessFlags'    :: !(BitSet16 MAccessFlag)
  , mName :: !(Ref Text.Text r)
  , mDescriptor :: !(Ref MethodDescriptor r)
  , mAttributes      :: !(Choice r (SizedList16 (Attribute r)) (MethodAttributes r))
  }

-- | Unpack the BitSet and get the AccessFlags as a Set.
mAccessFlags :: Method r -> Set MAccessFlag
mAccessFlags = toSet . mAccessFlags'

data MethodAttributes r = MethodAttributes
  { maCode       :: [Code r]
  , maExceptions :: [Exceptions r]
  , maSignatures :: [Signature r]
  , maOthers     :: [Attribute r]
  }

-- | Fetch the 'Code' attribute, if any.
-- There can only be one code attribute in a method.
mCode :: Method High -> Maybe (Code High)
mCode =
  firstOne . maCode . mAttributes

-- | Fetch the 'Exceptions' attribute.
-- There can only be one exceptions attribute in a method.
mExceptions' :: Method High -> Maybe (Exceptions High)
mExceptions' =
  firstOne . maExceptions . mAttributes

-- | Fetches the 'Exceptions' attribute, but turns it into an list of exceptions.
-- If no exceptions field where found the empty list is returned
mExceptions :: Method High -> [ClassName]
mExceptions =
  fromMaybe [] . fmap (unSizedList . exceptions) . mExceptions'

-- | Fetches the 'Signature' attribute, if any.
mSignature :: Method High -> Maybe (Signature High)
mSignature =
  firstOne . maSignatures . mAttributes

instance Staged Method where
  evolve (Method mf mn md mattr) = do
    mn' <- link mn
    md' <- link md
    mattr' <- label (Text.unpack (mn' <> ":" <> methodDescriptorToText md'))
      $ fromCollector <$> fromAttributes collect' mattr
    return $ Method mf mn' md' mattr'
    where
      fromCollector (a, b, c, d) =
        MethodAttributes (appEndo a []) (appEndo b []) (appEndo c []) (appEndo d [])
      collect' attr =
        collect (mempty, mempty, mempty, Endo (attr:)) attr
          [ toC $ \e -> (Endo (e:), mempty, mempty, mempty)
          , toC $ \e -> (mempty, Endo (e:), mempty, mempty)
          , toC $ \e -> (mempty, mempty, Endo (e:), mempty)
          ]

  devolve (Method mf mn md mattr) = do
    mn' <- unlink mn
    md' <- unlink md
    mattr' <- fromMethodAttributes $ mattr
    return $ Method mf mn' md' (SizedList mattr')
    where
      fromMethodAttributes (MethodAttributes a b c d) = do
        a' <- mapM toAttribute a
        b' <- mapM toAttribute b
        c' <- mapM toAttribute c
        d' <- mapM devolve d
        return (a' ++ b' ++ c' ++ d')

$(deriveBase ''MethodAttributes)
$(deriveBaseWithBinary ''Method)
