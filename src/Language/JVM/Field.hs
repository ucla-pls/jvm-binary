{-|
Module      : Language.JVM.Field
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.JVM.Field
  ( Field (..)
  , fName
  , fDescriptor
  , fAccessFlags
  -- * Attributes
 -- , fConstantValue
  ) where

import           Control.DeepSeq         (NFData)
import           Data.Binary
import           GHC.Generics            (Generic)

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Constant
import           Language.JVM.Utils

import qualified Data.Set                as Set

import qualified Data.Text               as Text

-- | A Field in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5).
data Field r = Field
  { fAccessFlags'    :: BitSet16 FAccessFlag
  , fNameIndex       :: Ref r Text.Text
  , fDescriptorIndex :: Ref r FieldDescriptor
  , fAttributes      :: SizedList16 (Attribute r)
  }

deriving instance Reference r => Show (Field r)
deriving instance Reference r => Eq (Field r)
deriving instance Reference r => Generic (Field r)
deriving instance Reference r => NFData (Field r)

deriving instance Binary (Field Index)

-- | Get the name of the field
fName :: Field Deref -> Text.Text
fName = valueF fNameIndex

-- | Get the set of access flags
fAccessFlags :: Field r -> Set.Set FAccessFlag
fAccessFlags = toSet . fAccessFlags'

-- | Get the descriptor of the field
fDescriptor :: Field Deref -> FieldDescriptor
fDescriptor = valueF fDescriptorIndex

-- -- | Fetch the 'ConstantValue' attribute.
-- -- There can only one be one exceptions attribute on a field.
-- fConstantValue :: Field -> PoolAccess (Maybe (Either String ConstantValue))
-- fConstantValue =
--   fmap firstOne . matching fAttributes

instance ClassFileReadable Field where
  untie field cp = do
    fi <- deref (fNameIndex field) cp
    fd <- deref (fDescriptorIndex field) cp
    fattr <- mapM (flip untie cp) (fAttributes field)
    return $ Field (fAccessFlags' field) fi fd fattr
