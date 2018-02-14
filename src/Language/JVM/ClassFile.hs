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

import           Control.DeepSeq (NFData)
import           GHC.Generics            (Generic)

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Constant
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

deriving instance Reference r => Show (ClassFile r)
deriving instance Reference r => Eq (ClassFile r)
deriving instance Reference r => Generic (ClassFile r)
deriving instance Reference r => NFData (ClassFile r)

deriving instance Binary (ClassFile Index)

-- | Get the set of access flags
cAccessFlags :: ClassFile r -> Set CAccessFlag
cAccessFlags = toSet . cAccessFlags'

-- | Get a list of 'ConstantRef's to interfaces.
cInterfaceIndicies :: ClassFile r -> [ Ref r ClassName ]
cInterfaceIndicies = unSizedList . cInterfaceIndicies'

-- | Get a list of 'ClassName'
cInterfaces :: ClassFile Deref -> [ ClassName ]
cInterfaces = Prelude.map (refValue) .  cInterfaceIndicies

-- | Get a list of 'Field's of a ClassFile.
cFields :: ClassFile r -> [Field r]
cFields = unSizedList . cFields'

-- | Get a list of 'Method's of a ClassFile.
cMethods :: ClassFile r -> [Method r]
cMethods = unSizedList . cMethods'

-- | Lookup the this class in a ConstantPool
cThisClass :: ClassFile Deref -> ClassName
cThisClass = valueF cThisClassIndex

-- | Lookup the super class in the ConstantPool
cSuperClass :: ClassFile Deref -> ClassName
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

instance ClassFileReadable ClassFile where
  untie cf cp = do
    tci' <- deref (cThisClassIndex cf) cp
    sci' <- deref (cSuperClassIndex cf) cp
    cii' <- mapM (flip deref cp) $ cInterfaceIndicies' cf
    cf' <- mapM (flip untie cp) $ cFields' cf
    cm' <- mapM (flip untie cp) $ cMethods' cf
    ca' <- mapM (flip untie cp) $ cAttributes' cf
    return $ cf
      { cConstantPool = cp -- set The Constant Pool to the Botstrapped one
      , cThisClassIndex = tci'
      , cSuperClassIndex = sci'
      , cInterfaceIndicies' = cii'
      , cFields'            = cf'
      , cMethods'           = cm'
      , cAttributes'        = ca'
      }
