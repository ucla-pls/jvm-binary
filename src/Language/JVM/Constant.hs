{-|
Module      : Language.JVM.Constant
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'Constant' type and the 'ConstantPool'. These
are essential for accessing data in the class-file.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE GADTs    #-}
module Language.JVM.Constant
  (
    -- * Constant
    Constant (..)
  , constantSize

    -- ** Special constants
  , InClass (..)
  , MethodId (..)
  , FieldId (..)
  , ClassName (..)

  , MethodDescriptor
  , FieldDescriptor

    -- * Constant Pool
    -- $ConstantPool
  , ConstantPool (..)

  , ConstantRef (..)
  , Index (..)
  , InConstantPool (..)

  , toListOfConstants

  ) where

import           GHC.Generics       (Generic)

import           Prelude            hiding (fail, lookup)

import           Control.Monad      (forM_)
import           Control.Monad.Fail (fail)

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.IntMap.Strict as IM

import           Language.JVM.Utils


import qualified Data.Text          as Text
import qualified Data.Text.Encoding as TE

-- | A constant is referenced by the ConstantRef data type. It is
-- just a 16 bit word, but wrapped for type safety.
newtype ConstantRef =
  ConstantRef Word16
  deriving (Eq, Show, Generic)

instance Binary ConstantRef where

-- | A constant is a multi word item in the 'ConstantPool'. Each of
-- the constructors are pretty much self expiatory from the types.
data Constant
  = String !SizedByteString16
  | Integer !Word32
  | Float !Word32
  | Long !Word64
  | Double !Word64
  | ClassRef (Index Text.Text)
  | StringRef (Index Text.Text)
  | FieldRef (Index ClassName) (Index FieldId)
  | MethodRef (Index ClassName) (Index MethodId)
  | InterfaceMethodRef (Index ClassName) (Index MethodId)
  | NameAndType (Index Text.Text) (Index Text.Text)
  | MethodHandle !Word8 !ConstantRef
  | MethodType (Index MethodDescriptor)
  | InvokeDynamic !Word16 !ConstantRef
  deriving (Show, Eq)

instance Binary Constant where
  get = do
    ident <- getWord8
    case ident of
      1  -> String <$> get
      3  -> Integer <$> get
      4  -> Float <$> get
      5  -> Long <$> get
      6  -> Double <$> get
      7  -> ClassRef <$> get
      8  -> StringRef <$> get
      9  -> FieldRef <$> get <*> get
      10 -> MethodRef <$> get <*> get
      11 -> InterfaceMethodRef <$> get <*> get
      12 -> NameAndType <$> get <*> get
      15 -> MethodHandle <$> get <*> get
      16 -> MethodType <$> get
      18 -> InvokeDynamic <$> get <*> get
      _  -> fail $ "Unkown identifier " ++ show ident

  put x =
    case x of
      String bs              -> do putWord8 1; put bs
      Integer i              -> do putWord8 3; put i
      Float i                -> do putWord8 4; put i
      Long i                 -> do putWord8 5; put i
      Double i               -> do putWord8 6; put i
      ClassRef i             -> do putWord8 7; put i
      StringRef i            -> do putWord8 8; put i
      FieldRef i j           -> do putWord8 9; put i; put j
      MethodRef i j          -> do putWord8 10; put i; put j
      InterfaceMethodRef i j -> do putWord8 11; put i; put j
      NameAndType i j        -> do putWord8 12; put i; put j
      MethodHandle i j       -> do putWord8 15; put i; put j
      MethodType i           -> do putWord8 16; put i;
      InvokeDynamic i j      -> do putWord8 18; put i; put j

-- | Some of the 'Constant's take up more space in the constant pool than other.
-- Notice that 'Language.JVM.Constant.String' and 'MethodType' is not of size
-- 32, but is still awarded value 1. This is due to an
-- [inconsistency](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.5)
-- in JVM.
constantSize :: Constant -> Int
constantSize x =
  case x of
    Double _ -> 2
    Long _   -> 2
    _        -> 1

-- | A class name
newtype ClassName = ClassName
  { classNameAsText :: Text.Text
  } deriving (Show, Eq, Ord)

-- | Any thing pointing inside a class
data InClass a = InClass
  { inClassName :: ClassName
  , inClassId :: a
  } deriving (Show, Eq, Ord)

-- | A method identifier
data MethodId = MethodId
  { methodIdName :: Text.Text
  , methodIdDescription :: MethodDescriptor
  } deriving (Show, Eq, Ord)

-- | A field identifier
data FieldId = FieldId
  { fieldIdName :: Text.Text
  , fieldIdDescription :: FieldDescriptor
  } deriving (Show, Eq, Ord)

-- | Method Descriptor
type MethodDescriptor = Text.Text

-- | Field Descriptor
type FieldDescriptor = Text.Text


-- $ConstantPool
--
-- The 'ConstantPool' contains all the constants, and is accessible using the
-- Lookup methods.

-- | A ConstantPool is just an 'IntMap'. A 'IntMap' is used, because constants are
-- accessed after their byte-offset. 'constantSize'
newtype ConstantPool = ConstantPool
  { unConstantPool :: IM.IntMap Constant
  } deriving (Show, Eq)

-- | Return a list of reference-constant pairs.
toListOfConstants :: ConstantPool -> [(ConstantRef, Constant)]
toListOfConstants =
  map (\(a,b) -> (ConstantRef . fromIntegral $ a, b)) . IM.toList . unConstantPool

instance Binary ConstantPool where
  get = do
    len <- fromIntegral <$> getInt16be
    list <- go len 1
    return . ConstantPool $ IM.fromList list
    where
      go len n | len > n = do
        constant <- get
        rest <- go len (n + constantSize constant)
        return $ (n, constant) : rest
      go _ _ = return []
  put (ConstantPool p)= do
    case IM.maxViewWithKey p of
      Just ((key, e), _) -> do
        putInt16be (fromIntegral (key + constantSize e))
        forM_ (IM.toAscList p) (put . snd)
      Nothing -> do
        putInt16be 0

-- | Lookup a 'Constant' in the 'ConstantPool'.
derefConstant :: ConstantPool -> ConstantRef -> Maybe Constant
derefConstant (ConstantPool cp) (ConstantRef ref) =
  IM.lookup (fromIntegral ref) cp

-- This part contains helpers to describe some of the semantics of the
-- class file.

-- | Describes an index into the constant pool
data Index x where
  Index :: (InConstantPool x) => ConstantRef -> Index x

deriving instance (Show (Index x))
deriving instance (Eq (Index x))

instance (InConstantPool x) => Binary (Index x) where
  get = Index <$> get
  put (Index ref) = put ref

-- | Describes if a type is in the constant pool
class InConstantPool a where
  deref :: ConstantPool -> Index a -> Maybe a

instance InConstantPool Constant where
  deref cp (Index ref) =
    derefConstant cp ref

-- | Lookup a 'Text.Text' in the 'ConstantPool', returns 'Nothing' if the
-- reference does not point to something in the ConstantPool, if it points to
-- something not a 'Language.JVM.Constant.String' Constant, or if it is
-- impossible to decode the 'ByteString' as Utf8.
instance InConstantPool Text.Text where
  deref cp (Index ref) = do
    String str <- derefConstant cp ref
    case TE.decodeUtf8' . unSizedByteString $ str of
      Left _    -> Nothing
      Right txt -> Just txt

instance InConstantPool ClassName where
  deref cp (Index ref) = do
    ClassRef r <- derefConstant cp ref
    ClassName <$> deref cp r

instance InConstantPool (InClass MethodId) where
  deref cp (Index ref) = do
    MethodRef cn rt <- derefConstant cp ref
    InClass <$> deref cp cn <*> deref cp rt

instance InConstantPool (InClass FieldId) where
  deref cp (Index ref) = do
    FieldRef cn rt <- derefConstant cp ref
    InClass <$> deref cp cn <*> deref cp rt

instance InConstantPool MethodId where
  deref cp (Index ref) = do
    NameAndType n t <- derefConstant cp ref
    MethodId <$> deref cp n <*> deref cp t

instance InConstantPool FieldId where
  deref cp (Index ref) = do
    NameAndType n t <- derefConstant cp ref
    FieldId <$> deref cp n <*> deref cp t
