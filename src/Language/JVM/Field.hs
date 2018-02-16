{-# LANGUAGE TemplateHaskell #-}
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

import qualified Data.Set                as Set
import qualified Data.Text               as Text

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.ConstantPool
import           Language.JVM.Utils

-- | A Field in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5).
data Field r = Field
  { fAccessFlags'    :: BitSet16 FAccessFlag
  , fNameIndex       :: Ref r Text.Text
  , fDescriptorIndex :: Ref r FieldDescriptor
  , fAttributes      :: SizedList16 (Attribute r)
  }

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

instance Staged Field where
  stage f field = do
    fi <- f (fNameIndex field)
    fd <- f (fDescriptorIndex field)
    fattr <- mapM (stage f) (fAttributes field)
    return $ Field (fAccessFlags' field) fi fd fattr

$(deriveBaseB ''Index ''Field)
