{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.JVM.Attribute
  ( Attribute (..)
  , aInfo'

  , aName

  , IsAttribute (..)
  -- SubAttributes
--  , module Language.JVM.Attribute.Code
  , Code

  ) where

import           GHC.Generics                (Generic)

import           Data.Bifunctor
import           Data.Binary
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.Text                   as Text

import           Language.JVM.Constant       (ConstantPool, ConstantRef,
                                              lookupText)
import           Language.JVM.Utils          (SizedByteString32 (..), trd)

import qualified Language.JVM.Attribute.Code

data Attribute = Attribute
  { aNameIndex :: ! ConstantRef
  , aInfo      :: ! SizedByteString32
  } deriving (Show, Eq, Generic)

instance Binary Attribute where


-- | A small helper function to extract the info as a
-- lazy `ByteString`.
aInfo' :: Attribute -> BS.ByteString
aInfo' = unSizedByteString32 . aInfo

aName :: ConstantPool -> Attribute -> Maybe Text.Text
aName cp as = lookupText (aNameIndex as) cp


newtype Const a b = Const { unConst :: a }

class IsAttribute a where
  attrName :: Const Text.Text a
  fromAttribute :: Attribute -> Either String a

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

type Code = Language.JVM.Attribute.Code.Code Attribute

instance IsAttribute Code where
  attrName = Const "Code"
  fromAttribute =
    bimap trd trd . decodeOrFail . BL.fromStrict . aInfo'
