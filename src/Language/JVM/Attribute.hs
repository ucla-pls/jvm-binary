{-|
Module      : Language.JVM.Attribute
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This is the main module for accessing all kinds of Attributes.
-}

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.JVM.Attribute
  ( Attribute (..)
  , aInfo

  , aName

  , IsAttribute (..)
  -- * SubAttributes
  , Code
  , codeStackMapTable
  , ConstantValue
  , Exceptions
  , StackMapTable
  , BootstrapMethods

  -- * Helpers
  , Const
  ) where

import           GHC.Generics                            (Generic)

import           Data.Bifunctor
import           Data.Binary
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as BL
import           Data.Monoid
import           Data.Text                               as Text

import           Language.JVM.Constant                   (ConstantPool,
                                                          ConstantRef,
                                                          lookupText)
import           Language.JVM.Utils                      (SizedByteString32,
                                                          trd,
                                                          unSizedByteString)

import           Language.JVM.Attribute.BootstrapMethods (BootstrapMethods)
import qualified Language.JVM.Attribute.Code             as Code
import           Language.JVM.Attribute.ConstantValue    (ConstantValue)
import           Language.JVM.Attribute.Exceptions       (Exceptions)
import           Language.JVM.Attribute.StackMapTable    (StackMapTable)

-- | An Attribute, simply contains of a reference to a name and
-- contains info.
data Attribute = Attribute
  { aNameIndex :: ! ConstantRef
  , aInfo'     :: ! SizedByteString32
  } deriving (Show, Eq, Generic)

instance Binary Attribute where

-- | A small helper function to extract the info as a
-- lazy 'Data.ByteString.Lazy.ByteString'.
aInfo :: Attribute -> BS.ByteString
aInfo = unSizedByteString . aInfo'

-- | Extracts the name from the attribute, if it exists in the
-- ConstantPool.
aName :: ConstantPool -> Attribute -> Maybe Text.Text
aName cp as = lookupText (aNameIndex as) cp

-- | Create a type dependent on another type 'b',
-- used for accessing the correct 'attrName' in 'IsAttribute'.
newtype Const a b = Const { unConst :: a }

-- | A class-type that describes a data-type 'a' as an Attribute. Most notable
-- it provides the 'fromAttribute'' method that enables converting an Attribute
-- to a data-type 'a'.
class IsAttribute a where
  attrName :: Const Text.Text a
  -- ^ The name of the attribute.

  fromAttribute :: Attribute -> Either String a
  -- ^ Generate a 'a' from an Attribute.

  fromAttribute'
    :: ConstantPool
    -> Attribute
    -> Maybe (Either String a)
  fromAttribute' cp as = do
    name <- aName cp as
    if name == unConst (attrName :: Const Text.Text a) then
       return $ fromAttribute as
    else Nothing

readFromStrict :: Binary a => Attribute -> Either String a
readFromStrict =
    bimap trd trd . decodeOrFail . BL.fromStrict . aInfo

-- # Attributes


-- | 'Code' is an Attribute.
instance IsAttribute Code where
  attrName = Const "Code"
  fromAttribute = readFromStrict

-- | 'ConstantValue' is an Attribute.
instance IsAttribute ConstantValue where
  attrName = Const "ConstantValue"
  fromAttribute = readFromStrict

-- | 'Exceptions' is an Attribute.
instance IsAttribute Exceptions where
  attrName = Const "Exceptions"
  fromAttribute = readFromStrict

-- | 'StackMapTable' is an Attribute.
instance IsAttribute StackMapTable where
  attrName = Const "StackMapTable"
  fromAttribute = readFromStrict

-- | 'BootstrapMethods' is an Attribute.
instance IsAttribute BootstrapMethods where
  attrName = Const "BootstrapMethods"
  fromAttribute = readFromStrict

-- | Code is redefined with Attribute, as it is recursively containing
-- 'Attribute'. This is a small hack to fix it.
type Code = Code.Code Attribute

-- | The a attribute of the code is the StackMapTable. There can be at most one.
-- If the version number of the file is more than 50, and there is no StackMapTable.
-- there is an implicit empty StackMapTable.
codeStackMapTable :: ConstantPool -> Code -> Maybe (Either String BootstrapMethods)
codeStackMapTable cp =
  getFirst . foldMap (First . fromAttribute' cp) . Code.attributes
