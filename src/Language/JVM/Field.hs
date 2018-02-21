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
import           Language.JVM.Stage
import           Language.JVM.Constant
import           Language.JVM.Utils

-- | A Field in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5).
data Field r = Field
  { fAccessFlags'    :: BitSet16 FAccessFlag
  , fNameIndex       :: Ref Text.Text r
  , fDescriptorIndex :: Ref FieldDescriptor r
  , fAttributes      :: SizedList16 (Attribute r)
  }

-- | Get the name of the field
fName :: Field High -> Text.Text
fName = valueF fNameIndex

-- | Get the set of access flags
fAccessFlags :: Field r -> Set.Set FAccessFlag
fAccessFlags = toSet . fAccessFlags'

-- | Get the descriptor of the field
fDescriptor :: Field High -> FieldDescriptor
fDescriptor = valueF fDescriptorIndex

-- -- | Fetch the 'ConstantValue' attribute.
-- -- There can only one be one exceptions attribute on a field.
-- fConstantValue :: Field -> PoolAccess (Maybe (Either String ConstantValue))
-- fConstantValue =
--   fmap firstOne . matching fAttributes

instance Staged Field where
  evolve field = do
    fi <- evolve (fNameIndex field)
    fd <- evolve (fDescriptorIndex field)
    fattr <- mapM evolve (fAttributes field)
    return $ Field (fAccessFlags' field) fi fd fattr
  devolve field = do
    fi <- devolve (fNameIndex field)
    fd <- devolve (fDescriptorIndex field)
    fattr <- mapM devolve (fAttributes field)
    return $ Field (fAccessFlags' field) fi fd fattr

$(deriveBaseWithBinary ''Field)
