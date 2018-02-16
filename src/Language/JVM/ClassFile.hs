{-# LANGUAGE TemplateHaskell #-}
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
  , cAttributes

  , cThisClass
  , cSuperClass

  -- * Attributes
  -- , cBootstrapMethods
  ) where

import           Data.Binary
import           Data.Set

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.ConstantPool
import           Language.JVM.Field      (Field)
import           Language.JVM.Method     (Method)
import           Language.JVM.Utils

-- | A 'ClassFile' as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html).

data ClassFile r = ClassFile
  { cMagicNumber        :: !Word32

  , cMinorVersion       :: !Word16
  , cMajorVersion       :: !Word16

  , cConstantPool       :: !(ConstantPool r)

  , cAccessFlags'       :: BitSet16 CAccessFlag

  , cThisClassIndex     :: Ref r ClassName
  , cSuperClassIndex    :: Ref r ClassName

  , cInterfaceIndicies' :: SizedList16 (Ref r ClassName)
  , cFields'            :: SizedList16 (Field r)
  , cMethods'           :: SizedList16 (Method r)
  , cAttributes'        :: SizedList16 (Attribute r)
  }

-- | Get the set of access flags
cAccessFlags :: ClassFile r -> Set CAccessFlag
cAccessFlags = toSet . cAccessFlags'

-- | Get a list of 'ConstantRef's to interfaces.
cInterfaceIndicies :: ClassFile r -> [ Ref r ClassName ]
cInterfaceIndicies = unSizedList . cInterfaceIndicies'

-- | Get a list of 'ClassName'
cInterfaces :: (WithValue r) => ClassFile r -> [ ClassName ]
cInterfaces = Prelude.map getValue . cInterfaceIndicies

-- | Get a list of 'Field's of a ClassFile.
cFields :: ClassFile r -> [Field r]
cFields = unSizedList . cFields'

-- | Get a list of 'Method's of a ClassFile.
cMethods :: ClassFile r -> [Method r]
cMethods = unSizedList . cMethods'

-- | Lookup the this class in a ConstantPool
cThisClass :: (WithValue r) => ClassFile r -> ClassName
cThisClass = valueF cThisClassIndex

-- | Lookup the super class in the ConstantPool
cSuperClass :: WithValue r => ClassFile r -> ClassName
cSuperClass = valueF cSuperClassIndex

-- | Get a list of 'Attribute's of a ClassFile.
cAttributes :: ClassFile r -> [Attribute r]
cAttributes = unSizedList . cAttributes'

-- -- | Fetch the 'BootstrapMethods' attribute.
-- -- There can only one bootstrap methods per class, but there might not be
-- -- one.
-- cBootstrapMethods :: ClassFile -> PoolAccess (Maybe (Either String BootstrapMethods))
-- cBootstrapMethods =
--   fmap firstOne . matching cAttributes'

instance Staged ClassFile where
  stage f cf = do
    tci' <- f (cThisClassIndex cf)
    sci' <- f (cSuperClassIndex cf)
    cp' <- stage f (cConstantPool cf)
    cii' <- mapM f $ cInterfaceIndicies' cf
    cf' <- mapM (stage f) $ cFields' cf
    cm' <- mapM (stage f) $ cMethods' cf
    ca' <- mapM (stage f) $ cAttributes' cf
    return $ cf
      { cConstantPool = cp' -- set The Constant Pool to the Botstrapped one
      , cThisClassIndex = tci'
      , cSuperClassIndex = sci'
      , cInterfaceIndicies' = cii'
      , cFields'            = cf'
      , cMethods'           = cm'
      , cAttributes'        = ca'
      }

$(deriveBaseB ''Index ''ClassFile)
