{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Language.JVM.Method
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.JVM.Method
  ( Method (..)

  , mAccessFlags
  , mName
  , mDescriptor
  , mAttributes

  -- * Attributes
  -- , mCode
  -- , mExceptions'
  -- , mExceptions

  ) where

import           Data.Set (Set)
import qualified Data.Text as Text

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
-- import           Language.JVM.Attribute.Exceptions (exceptionIndexTable)
import           Language.JVM.Stage
import           Language.JVM.Constant
import           Language.JVM.Utils

-- | A Method in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6).
data Method r = Method
  { mAccessFlags'    :: BitSet16 MAccessFlag
  , mNameIndex       :: Ref Text.Text r
  , mDescriptorIndex :: Ref MethodDescriptor r
  , mAttributes'     :: SizedList16 (Attribute r)
  }

-- | Unpack the BitSet and get the AccessFlags as a Set.
mAccessFlags :: Method r -> Set MAccessFlag
mAccessFlags = toSet . mAccessFlags'

-- | Unpack the SizedList and get the attributes as a List.
mAttributes :: Method r -> [Attribute r]
mAttributes = unSizedList . mAttributes'

-- | Lookup the name of the method in the 'ConstantPool'.
mName :: Method High -> Text.Text
mName = valueF mNameIndex

-- | Lookup the descriptor of the method in the 'ConstantPool'.
mDescriptor :: Method High -> MethodDescriptor
mDescriptor = valueF mDescriptorIndex

-- mCode :: (WithValue r) => Method r -> Maybe (Code r)
-- mCode = . choseValue . mAttributes'

data MethodAttributes r = MethodAttributes
  { maCode :: Maybe (Code r)
  , maExceptions :: Maybe (Exceptions r)
  , maAttributes :: [Attribute r]
  }

-- -- | Fetch the 'Code' attribute, if any.
-- -- There can only be one code attribute in a method.
-- mCode :: Method -> PoolAccess (Maybe (Either String Code))
-- mCode = fmap firstOne . matching mAttributes

-- -- | Fetch the 'Exceptions' attribute.
-- -- There can only be one exceptions attribute in a method.
-- mExceptions' :: Method -> PoolAccess (Maybe (Either String Exceptions))
-- mExceptions' = fmap firstOne . matching mAttributes

-- -- | Fetches the 'Exceptions' attribute, but turns it into an list of exceptions.
-- -- If no exceptions field where found the empty list is returned
-- mExceptions :: Method -> PoolAccess (Either String [ClassName])
-- mExceptions m = do
--   exs <- mExceptions' m
--   case exs of
--     Just (Right a) ->
--       Right <$> mapM deref (exceptionIndexTable a)
--     Just (Left msg) ->
--       return $ Left msg
--     Nothing ->
--       return $ Right []

instance Staged Method where
  evolve (Method mf mn md mattr) = do
    mn' <- evolve mn
    md' <- evolve md
    mattr' <- mapM evolve mattr
    return $ Method mf mn' md' mattr'

  devolve (Method mf mn md mattr) = do
    mn' <- devolve mn
    md' <- devolve md
    mattr' <- mapM devolve mattr
    return $ Method mf mn' md' mattr'

$(deriveBaseWithBinary ''Method)
