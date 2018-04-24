{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module      : Language.JVM.ClassFile
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

The class file is described in this module.
-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.JVM.ClassFile
  ( ClassFile (..)

  , cAccessFlags
  , cInterfaceIndicies
  , cInterfaces
  , cFields
  , cMethods
  , cSignature

  , cThisClass
  , cSuperClass

  -- * Attributes
  , ClassAttributes (..)
  , cBootstrapMethods
  ) where

import           Data.Binary
import           Data.Monoid
import           Data.Set

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Attribute.BootstrapMethods
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
  { cMagicNumber        :: !Word32

  , cMinorVersion       :: !Word16
  , cMajorVersion       :: !Word16

  , cConstantPool       :: !(Choice r (ConstantPool r) ())

  , cAccessFlags'       :: BitSet16 CAccessFlag

  , cThisClassIndex     :: Ref ClassName r
  , cSuperClassIndex    :: Ref ClassName r

  , cInterfaceIndicies' :: SizedList16 (Ref ClassName r)
  , cFields'            :: SizedList16 (Field r)
  , cMethods'           :: SizedList16 (Method r)
  , cAttributes         :: Choice r (SizedList16 (Attribute r)) (ClassAttributes r)
  }

-- | Get the set of access flags
cAccessFlags :: ClassFile r -> Set CAccessFlag
cAccessFlags = toSet . cAccessFlags'

-- | Get a list of 'ConstantRef's to interfaces.
cInterfaceIndicies :: ClassFile r -> [ Ref ClassName r ]
cInterfaceIndicies = unSizedList . cInterfaceIndicies'

-- | Get a list of 'ClassName'
cInterfaces :: ClassFile High -> [ ClassName ]
cInterfaces = Prelude.map value . cInterfaceIndicies

-- | Get a list of 'Field's of a ClassFile.
cFields :: ClassFile r -> [Field r]
cFields = unSizedList . cFields'

-- | Get a list of 'Method's of a ClassFile.
cMethods :: ClassFile r -> [Method r]
cMethods = unSizedList . cMethods'

-- | Lookup the this class in a ConstantPool
cThisClass :: ClassFile High -> ClassName
cThisClass = value . cThisClassIndex

-- | Lookup the super class in the ConstantPool
cSuperClass :: ClassFile High -> ClassName
cSuperClass = value . cSuperClassIndex

-- -- | Get a list of 'Attribute's of a ClassFile.
-- cAttributes :: ClassFile r -> [Attribute r]
-- cAttributes = unSizedList . cAttributes'

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

data ClassAttributes r = ClassAttributes
  { caBootstrapMethods :: [ BootstrapMethods r]
  , caSignature        :: [ Signature r ]
  , caOthers           :: [ Attribute r ]
  }

instance Staged ClassFile where
  evolve cf = label "ClassFile" $ do
    tci' <- evolve (cThisClassIndex cf)
    sci' <-
      if value tci' /= ClassName "java/lang/Object"
      then do
        evolve (cSuperClassIndex cf)
      else do
        return $ RefV (ClassName "java/lang/Object")
    cii' <- mapM evolve $ cInterfaceIndicies' cf
    cf' <- mapM evolve $ cFields' cf
    cm' <- mapM evolve $ cMethods' cf
    ca' <- fromCollector <$> fromAttributes collect' (cAttributes cf)
    return $ cf
      { cConstantPool = ()
      , cThisClassIndex = tci'
      , cSuperClassIndex = sci'
      , cInterfaceIndicies' = cii'
      , cFields'            = cf'
      , cMethods'           = cm'
      , cAttributes         = ca'
      }
    where
      fromCollector (a, b, c) =
        ClassAttributes (appEndo a []) (appEndo b []) (appEndo c [])
      collect' attr =
        collect (mempty, mempty, Endo (attr:)) attr
          [ toC $ \e -> (Endo (e:), mempty, mempty)
          , toC $ \e -> (mempty, Endo (e:), mempty)]

  devolve cf = do
    tci' <- devolve (cThisClassIndex cf)
    sci' <-
      if cThisClass cf /= ClassName "java/lang/Object" then
        devolve (cSuperClassIndex cf)
      else
        return $ RefI 0
    cii' <- mapM devolve $ cInterfaceIndicies' cf
    cf' <- mapM devolve $ cFields' cf
    cm' <- mapM devolve $ cMethods' cf
    ca' <- fromClassAttributes $ cAttributes cf
    return $ cf
      { cConstantPool       = CP.empty
      -- ^ We cannot yet set the constant pool
      , cThisClassIndex     = tci'
      , cSuperClassIndex    = sci'
      , cInterfaceIndicies' = cii'
      , cFields'            = cf'
      , cMethods'           = cm'
      , cAttributes         = SizedList ca'
      }
    where
      fromClassAttributes (ClassAttributes cm cs at) = do
        cm' <- mapM toAttribute cm
        cs' <- mapM toAttribute cs
        at' <- mapM devolve at
        return (cm' ++ cs' ++ at')


$(deriveBase ''ClassAttributes)
$(deriveBaseWithBinary ''ClassFile)
