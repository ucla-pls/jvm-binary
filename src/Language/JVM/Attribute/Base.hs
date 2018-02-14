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

import           Control.DeepSeq       (NFData)
import           Data.Binary
import           Data.Text             as Text
import           GHC.Generics          (Generic)

import qualified Data.ByteString       as BS

import           Language.JVM.Constant
import           Language.JVM.Utils    (SizedByteString32, unSizedByteString)

-- | An Attribute, simply contains of a reference to a name and
-- contains info.
data Attribute r = Attribute
  { aNameIndex :: ! (Ref r Text.Text)
  , aInfo'     :: ! (SizedByteString32)
  }

deriving instance Reference r => Show (Attribute r)
deriving instance Reference r => Eq (Attribute r)
deriving instance Reference r => Generic (Attribute r)
deriving instance Reference r => NFData (Attribute r)

instance Binary (Attribute Index) where

-- | A small helper function to extract the info as a
-- lazy 'Data.ByteString.Lazy.ByteString'.
aInfo :: Attribute r -> BS.ByteString
aInfo = unSizedByteString . aInfo'

-- | Extracts the name from the attribute, if it exists in the
-- ConstantPool.
aName :: Attribute Deref -> Text.Text
aName = valueF aNameIndex

instance ClassFileReadable Attribute where
  untie (Attribute an ai) cp = do
    an' <- deref an cp
    return $ Attribute an' ai
