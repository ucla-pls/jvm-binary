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

cBootstrapMethods :: ClassFile High -> Maybe [BootstrapMethod High]
cBootstrapMethods =
  fmap methods . cBootstrapMethods'

data ClassAttributes r = ClassAttributes
  { caBootstrapMethods :: [ BootstrapMethods r]
  , caOthers           :: [ Attribute r ]
  }

instance Staged ClassFile where
  evolve cf = do
    tci' <- evolve (cThisClassIndex cf)
    sci' <- evolve (cSuperClassIndex cf)
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
      fromCollector (a, b) =
        ClassAttributes (appEndo a []) (appEndo b [])
      collect' attr =
        collect (mempty, Endo (attr:)) attr
          [ toC $ \e -> (Endo (e:), mempty) ]

  devolve cf = do
    tci' <- devolve (cThisClassIndex cf)
    sci' <- devolve (cSuperClassIndex cf)
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
      fromClassAttributes (ClassAttributes cm at) = do
        cm' <- mapM toAttribute cm
        at' <- mapM devolve at
        return (cm' ++ at')


$(deriveBase ''ClassAttributes)
$(deriveBaseWithBinary ''ClassFile)
