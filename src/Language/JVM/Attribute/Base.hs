{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Language.JVM.Attribute.Base
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}
module Language.JVM.Attribute.Base
  ( Attribute (..)
  , aInfo
  , aName
  ) where

import           Data.Text             as Text
import qualified Data.ByteString       as BS

import           Language.JVM.ConstantPool
import           Language.JVM.Utils    (SizedByteString32, unSizedByteString)

-- | An Attribute, simply contains of a reference to a name and
-- contains info.
data Attribute r = Attribute
  { aNameIndex :: ! (Ref r Text.Text)
  , aInfo'     :: ! (SizedByteString32)
  }

-- | A small helper function to extract the info as a
-- lazy 'Data.ByteString.Lazy.ByteString'.
aInfo :: Attribute r -> BS.ByteString
aInfo = unSizedByteString . aInfo'

-- | Extracts the name from the attribute, if it exists in the
-- ConstantPool.
aName :: Attribute Deref -> Text.Text
aName = valueF aNameIndex

instance Staged Attribute where
  stage f (Attribute an ai) = do
    an' <- f an
    return $ Attribute an' ai

$(deriveBaseB ''Index ''Attribute)
