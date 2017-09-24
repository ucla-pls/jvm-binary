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
  -- SubAttributes
--  , module Language.JVM.Attribute.Code
  , Code

  , Const

  ) where

import           GHC.Generics                (Generic)

import           Data.Bifunctor
import           Data.Binary
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.Text                   as Text

import           Language.JVM.Constant       (ConstantPool, ConstantRef,
                                              lookupText)
import           Language.JVM.Utils          (SizedByteString32, trd,
                                              unSizedByteString)

import qualified Language.JVM.Attribute.Code

-- | An Attribute, simply contains of a reference to a name and
-- contains info.
data Attribute = Attribute
  { aNameIndex :: ! ConstantRef
  , aInfo'      :: ! SizedByteString32
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

-- # Attributes

-- | Code is redefined with Attribute, as it is recursively containing
-- 'Attribute'. This is a small hack to fix it.
type Code = Language.JVM.Attribute.Code.Code Attribute

-- | Code is an Attribute.
instance IsAttribute Code where
  attrName =
    Const "Code"
  fromAttribute =
    bimap trd trd . decodeOrFail . BL.fromStrict . aInfo
