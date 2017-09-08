{-# LANGUAGE DeriveGeneric #-}
module Language.JVM.ClassFile
  ( ClassFile (..)

  , cInterfaces'
  , cFields'
  , cMethods'
  , cAttributes'
  ) where

import           GHC.Generics            (Generic)
import           Data.Binary


import           Language.JVM.AccessFlag
import           Language.JVM.Attribute  (Attribute)
import           Language.JVM.Constant   (ConstantPool, ConstantRef)
import           Language.JVM.Field      (Field)
import           Language.JVM.Method     (Method)
import           Language.JVM.Utils

data ClassFile = ClassFile
  { cMagicNumber  :: !Word32

  , cMinorVersion :: !Word16
  , cMajorVersion :: !Word16

  , cConstantPool :: !ConstantPool

  , cAccessFlags  :: BitSet16 CAccessFlag

  , cThisClass    :: !ConstantRef
  , cSuperClass   :: !ConstantRef

  , cInterfaces   :: SizedList16 ConstantRef
  , cFields       :: SizedList16 Field
  , cMethods      :: SizedList16 Method
  , cAttributes   :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary ClassFile where

cInterfaces' :: ClassFile -> [ConstantRef]
cInterfaces' = unSizedList16 . cInterfaces

cFields' :: ClassFile -> [Field]
cFields' = unSizedList16 . cFields

cMethods' :: ClassFile -> [Method]
cMethods' = unSizedList16 . cMethods

cAttributes' :: ClassFile -> [Attribute]
cAttributes' = unSizedList16 . cAttributes
