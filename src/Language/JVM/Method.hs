{-|
Module      : Language.JVM.Method
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.JVM.Method
  ( Method (..)
  , mAccessFlags

  -- * Attributes
  , MethodAttributes (..)
  , emptyMethodAttributes
  , mCode
  , mExceptions'
  , mExceptions
  , mSignature

  ) where

-- base
import           Data.Monoid

-- containers
import           Data.Set                          (Set)

-- text
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
  { mAccessFlags' :: !(BitSet16 MAccessFlag)
  , mName         :: !(Ref Text.Text r)
  , mDescriptor   :: !(Ref MethodDescriptor r)
  , mAttributes   :: !(Attributes MethodAttributes r)
  }

-- | Unpack the BitSet and get the AccessFlags as a Set.
mAccessFlags :: Method r -> Set MAccessFlag
mAccessFlags = toSet . mAccessFlags'

data MethodAttributes r = MethodAttributes
  { maCode                          :: [Code r]
  , maExceptions                    :: [Exceptions r]
  , maSignatures                    :: [Signature r]
  , maAnnotationDefault             :: [AnnotationDefault r]
  , maVisibleAnnotations            :: [RuntimeVisibleAnnotations r]
  , maInvisibleAnnotations          :: [RuntimeInvisibleAnnotations r]
  , maVisibleParameterAnnotations   :: [RuntimeVisibleParameterAnnotations r]
  , maInvisibleParamterAnnotations  :: [RuntimeInvisibleParameterAnnotations r]
  , maVisibleTypeAnnotations            ::
      [RuntimeVisibleTypeAnnotations MethodTypeAnnotation r]
  , maInvisibleTypeAnnotations          ::
      [RuntimeInvisibleTypeAnnotations MethodTypeAnnotation r]
  , maOthers               :: [Attribute r]
  }

emptyMethodAttributes :: MethodAttributes High
emptyMethodAttributes =
  MethodAttributes [] [] [] [] [] [] [] [] [] [] []

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
  maybe [] (unSizedList . exceptions) . mExceptions'

-- | Fetches the 'Signature' attribute, if any.
mSignature :: Method High -> Maybe (Signature High)
mSignature =
  firstOne . maSignatures . mAttributes

instance Staged Method where
  evolve (Method mf mn md mattr) = label "Method" $ do
    mn' <- link mn
    md' <- link md
    label (Text.unpack.toText $ MethodId (NameAndType mn' md')) $ do
      mattr' <- fmap (`appEndo` emptyMethodAttributes) . fromAttributes MethodAttribute mattr
        $ collect
          [ Attr (\e a -> a { maCode = e : maCode a })
          , Attr (\e a -> a { maExceptions = e : maExceptions a })
          , Attr (\e a -> a { maSignatures = e : maSignatures a })
          , Attr (\e a -> a { maAnnotationDefault = e : maAnnotationDefault a })
          , Attr (\e a -> a { maVisibleAnnotations = e : maVisibleAnnotations a })
          , Attr (\e a -> a { maInvisibleAnnotations = e : maInvisibleAnnotations a })
          , Attr (\e a -> a { maVisibleParameterAnnotations = e : maVisibleParameterAnnotations a })
          , Attr (\e a -> a { maInvisibleParamterAnnotations = e : maInvisibleParamterAnnotations a })
          , Attr (\e a -> a { maVisibleTypeAnnotations = e : maVisibleTypeAnnotations a })
          , Attr (\e a -> a { maInvisibleTypeAnnotations = e : maInvisibleTypeAnnotations a })
          ]
          (\e a -> a { maOthers = e : maOthers a })
      return $ Method mf mn' md' mattr'

  devolve (Method mf mn md mattr) = do
    mn' <- unlink mn
    md' <- unlink md
    mattr' <- fromMethodAttributes mattr
    return $ Method mf mn' md' (SizedList mattr')
    where
      fromMethodAttributes MethodAttributes {..} =
        concat <$> sequence
          [ mapM toAttribute maCode
          , mapM toAttribute maExceptions
          , mapM toAttribute maSignatures
          , mapM toAttribute maAnnotationDefault
          , mapM toAttribute maVisibleAnnotations
          , mapM toAttribute maInvisibleAnnotations
          , mapM toAttribute maVisibleParameterAnnotations
          , mapM toAttribute maInvisibleParamterAnnotations
          , mapM toAttribute maVisibleTypeAnnotations
          , mapM toAttribute maInvisibleTypeAnnotations
          , mapM devolve maOthers
          ]

$(deriveBase ''MethodAttributes)
$(deriveBaseWithBinary ''Method)
