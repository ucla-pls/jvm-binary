{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Language.JVM.ClassFile
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

The class file is described in this module.
-}
module Language.JVM.ClassFile
  ( ClassFile (..)
  , cAccessFlags
  , cFields
  , cMethods
  , cSignature
  , cEnclosingMethod
  , cInnerClasses

  -- * Attributes
  , ClassAttributes (..)
  , emptyClassAttributes
  , cBootstrapMethods
  ) where

import           Data.Binary
import           Data.Monoid
import           Data.Set

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Attribute.BootstrapMethods
import           Language.JVM.Attribute.EnclosingMethod
import           Language.JVM.Attribute.InnerClasses
-- import           Language.JVM.Attribute.Signature
import           Language.JVM.Constant
import           Language.JVM.ConstantPool               as CP
import           Language.JVM.Field                      (Field)
import           Language.JVM.Method                     (Method)
import           Language.JVM.Staged
import           Language.JVM.Utils

-- | A 'ClassFile' as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html).

data ClassFile r = ClassFile
  { cMagicNumber  :: !Word32

  , cMinorVersion :: !Word16
  , cMajorVersion :: !Word16

  , cConstantPool :: !(Choice (ConstantPool r) () r)

  , cAccessFlags' :: !(BitSet16 CAccessFlag)

  , cThisClass    :: !(Ref ClassName r)
  , cSuperClass   :: !(Ref ClassName r)

  , cInterfaces   :: !(SizedList16 (Ref ClassName r))
  , cFields'      :: !(SizedList16 (Field r))
  , cMethods'     :: !(SizedList16 (Method r))
  , cAttributes   :: !(Attributes ClassAttributes r)
  }

-- | Get the set of access flags
cAccessFlags :: ClassFile r -> Set CAccessFlag
cAccessFlags = toSet . cAccessFlags'

-- | Get a list of 'Field's of a ClassFile.
cFields :: ClassFile r -> [Field r]
cFields = unSizedList . cFields'

-- | Get a list of 'Method's of a ClassFile.
cMethods :: ClassFile r -> [Method r]
cMethods = unSizedList . cMethods'


-- | Fetch the 'BootstrapMethods' attribute.
-- There can only one bootstrap methods per class, but there might not be
-- one.
cBootstrapMethods' :: ClassFile High -> Maybe (BootstrapMethods High)
cBootstrapMethods' =
  firstOne . caBootstrapMethods . cAttributes

cBootstrapMethods :: ClassFile High -> [BootstrapMethod High]
cBootstrapMethods =
  maybe [] methods . cBootstrapMethods'

cSignature :: ClassFile High -> Maybe (Signature High)
cSignature =
  firstOne . caSignature . cAttributes

cEnclosingMethod :: ClassFile High -> Maybe (EnclosingMethod High)
cEnclosingMethod =
  firstOne . caEnclosingMethod . cAttributes

cInnerClasses' :: ClassFile High -> Maybe (InnerClasses High)
cInnerClasses' =
  firstOne . caInnerClasses . cAttributes

cInnerClasses :: ClassFile High -> [InnerClass High]
cInnerClasses =
  maybe [] innerClasses . cInnerClasses'

data ClassAttributes r = ClassAttributes
  { caBootstrapMethods     :: [ BootstrapMethods r]
  , caSignature            :: [ Signature r ]
  , caEnclosingMethod      :: [ EnclosingMethod r ]
  , caInnerClasses         :: [ InnerClasses r ]
  , caVisibleAnnotations   :: [ RuntimeVisibleAnnotations r ]
  , caInvisibleAnnotations :: [ RuntimeInvisibleAnnotations r ]
  , caVisibleTypeAnnotations   ::
      [ RuntimeVisibleTypeAnnotations ClassTypeAnnotation r ]
  , caInvisibleTypeAnnotations ::
      [ RuntimeInvisibleTypeAnnotations ClassTypeAnnotation r ]
  , caOthers               :: [ Attribute r ]
  }

emptyClassAttributes :: ClassAttributes High
emptyClassAttributes =
  ClassAttributes [] [] [] [] [] [] [] [] []

instance Staged ClassFile where
  evolve cf = label "ClassFile" $ do
    tci' <- link (cThisClass cf)
    sci' <-
      if tci' /= "java/lang/Object"
      then do
        link (cSuperClass cf)
      else do
        return $ "java/lang/Object"
    cii' <- mapM link $ cInterfaces cf
    cf' <- mapM evolve $ cFields' cf
    cm' <- mapM evolve $ cMethods' cf
    ca' <- fmap (`appEndo` emptyClassAttributes) . fromAttributes ClassAttribute (cAttributes cf)
      $ collect
      [ Attr $ \e ca -> ca {caSignature = e : caSignature ca}
      , Attr $ \e ca -> ca {caEnclosingMethod = e : caEnclosingMethod ca}
      , Attr $ \e ca -> ca {caBootstrapMethods = e : caBootstrapMethods ca}
      , Attr $ \e ca -> ca {caVisibleAnnotations = e : caVisibleAnnotations ca}
      , Attr $ \e ca -> ca {caInvisibleAnnotations = e : caInvisibleAnnotations ca}
      , Attr $ \e ca -> ca {caVisibleTypeAnnotations = e : caVisibleTypeAnnotations ca}
      , Attr $ \e ca -> ca {caInvisibleTypeAnnotations = e : caInvisibleTypeAnnotations ca}
      , Attr $ \e ca -> ca {caInnerClasses = e : caInnerClasses ca}
      ]
      (\e ca -> ca {caOthers = e : caOthers ca})
    return $ cf
      { cConstantPool = ()
      , cThisClass = tci'
      , cSuperClass = sci'
      , cInterfaces = cii'
      , cFields'            = cf'
      , cMethods'           = cm'
      , cAttributes         = ca'
      }

  devolve cf = do
    tci' <- unlink (cThisClass cf)
    sci' <-
      if cThisClass cf /= "java/lang/Object" then
        unlink (cSuperClass cf)
      else
        return $ 0
    cii' <- mapM unlink $ cInterfaces cf
    cf' <- mapM devolve $ cFields' cf
    cm' <- mapM devolve $ cMethods' cf
    ca' <- fromClassAttributes $ cAttributes cf
    return $ cf
      { cConstantPool       = CP.empty
      -- We cannot yet set the constant pool
      , cThisClass = tci'
      , cSuperClass = sci'
      , cInterfaces  = cii'
      , cFields'            = cf'
      , cMethods'           = cm'
      , cAttributes         = SizedList ca'
      }
    where
      fromClassAttributes (ClassAttributes {..}) = do
        concat <$> sequence
          [ mapM toAttribute caBootstrapMethods
          , mapM toAttribute caSignature
          , mapM toAttribute caEnclosingMethod
          , mapM toAttribute caInnerClasses
          , mapM toAttribute caVisibleAnnotations
          , mapM toAttribute caInvisibleAnnotations
          , mapM toAttribute caVisibleTypeAnnotations
          , mapM toAttribute caInvisibleTypeAnnotations
          , mapM devolve caOthers
          ]

$(deriveBase ''ClassAttributes)
$(deriveBaseWithBinary ''ClassFile)
