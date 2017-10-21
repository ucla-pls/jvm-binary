{-|
Module      : Language.JVM.ClassFile
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

The class file is described in this module.
-}

{-# LANGUAGE DeriveGeneric #-}
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
  , cBootstrapMethods
  ) where

import           Data.Binary
import           Data.Set
import           GHC.Generics            (Generic)

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Constant
import           Language.JVM.Field      (Field)
import           Language.JVM.Method     (Method)
import           Language.JVM.Utils

-- | A 'ClassFile' as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html).

data ClassFile = ClassFile
  { cMagicNumber        :: !Word32

  , cMinorVersion       :: !Word16
  , cMajorVersion       :: !Word16

  , cConstantPool       :: !ConstantPool

  , cAccessFlags'       :: BitSet16 CAccessFlag

  , cThisClassIndex     :: Index ClassName
  , cSuperClassIndex    :: Index ClassName

  , cInterfaceIndicies' :: SizedList16 (Index ClassName)
  , cFields'            :: SizedList16 Field
  , cMethods'           :: SizedList16 Method
  , cAttributes'        :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary ClassFile where

-- | Get the set of access flags
cAccessFlags :: ClassFile -> Set CAccessFlag
cAccessFlags = toSet . cAccessFlags'

-- | Get a list of 'ConstantRef's to interfaces.
cInterfaceIndicies :: ClassFile -> [ Index ClassName ]
cInterfaceIndicies = unSizedList . cInterfaceIndicies'

-- | Get a list of 'ClassName'
cInterfaces :: ClassFile -> PoolAccess [ ClassName ]
cInterfaces = mapM deref . cInterfaceIndicies

-- | Get a list of 'Field's of a ClassFile.
cFields :: ClassFile -> [Field]
cFields = unSizedList . cFields'

-- | Get a list of 'Method's of a ClassFile.
cMethods :: ClassFile -> [Method]
cMethods = unSizedList . cMethods'

-- | Lookup the this class in a ConstantPool
cThisClass :: ClassFile -> PoolAccess ClassName
cThisClass = derefF cThisClassIndex

-- | Lookup the super class in the ConstantPool
cSuperClass :: ClassFile -> PoolAccess ClassName
cSuperClass = derefF cSuperClassIndex

-- | Get a list of 'Attribute's of a ClassFile.
cAttributes :: ClassFile -> [Attribute]
cAttributes = unSizedList . cAttributes'

-- | Fetch the 'BootstrapMethods' attribute.
-- There can only one bootstrap methods per class, but there might not be
-- one.
cBootstrapMethods :: ClassFile -> PoolAccess (Maybe (Either String BootstrapMethods))
cBootstrapMethods =
  fmap firstOne . matching cAttributes'
