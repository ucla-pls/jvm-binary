{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings    #-}
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
  , mName
  , mDescriptor

  -- * Attributes
  , MethodAttributes (..)
  , mCode
  , mExceptions'
  , mExceptions
  , mSignature

  ) where

import           Data.Monoid
import           Data.Maybe
import           Data.Set                (Set)
import qualified Data.Text               as Text

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Attribute.Exceptions (exceptionIndexTable)
import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.Utils
import           Language.JVM.Type

-- | A Method in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6).
data Method r = Method
  { mAccessFlags'    :: BitSet16 MAccessFlag
  , mNameIndex       :: Ref Text.Text r
  , mDescriptorIndex :: Ref MethodDescriptor r
  , mAttributes      :: Choice r (SizedList16 (Attribute r)) (MethodAttributes r)
  }

-- | Unpack the BitSet and get the AccessFlags as a Set.
mAccessFlags :: Method r -> Set MAccessFlag
mAccessFlags = toSet . mAccessFlags'

-- | Lookup the name of the method in the 'ConstantPool'.
mName :: Method High -> Text.Text
mName = value . mNameIndex

-- | Lookup the descriptor of the method in the 'ConstantPool'.
mDescriptor :: Method High -> MethodDescriptor
mDescriptor = value .  mDescriptorIndex

data MethodAttributes r = MethodAttributes
  { maCode       :: [Code r]
  , maExceptions :: [Exceptions r]
  , maSignatures :: [Signature r]
  , maOthers :: [Attribute r]
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
  map value . fromMaybe [] . fmap exceptionIndexTable . mExceptions'

-- | Fetches the 'Signature' attribute, if any.
mSignature :: Method High -> Maybe (Signature High)
mSignature =
  firstOne . maSignatures . mAttributes

instance Staged Method where
  evolve (Method mf mn md mattr) = do
    mn' <- evolve mn
    md' <- evolve md
    mattr' <- label (Text.unpack (value mn' <> ":" <> methodDescriptorToText (value md')))
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
    mn' <- devolve mn
    md' <- devolve md
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
