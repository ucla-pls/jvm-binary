{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
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

  , Index(..)
  , InConstantPool (..)

  , PoolAccess(..)
  , PoolAccessError(..)

  , derefF

  ) where

import           Prelude            hiding (fail, lookup)

import           Control.Monad      (forM_)
import           Control.Monad.Fail (fail)

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.IntMap.Strict as IM

import           Language.JVM.Utils
import           Language.JVM.Type

import qualified Data.Text          as Text
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

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
  | MethodHandle !Word8 (Index Constant)
  | MethodType (Index MethodDescriptor)
  | InvokeDynamic !Word16 (Index Constant)
  deriving (Show, Eq)

typeToStr :: Constant -> String
typeToStr = head . words . show

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


-- $ConstantPool
-- The 'ConstantPool' contains all the constants, and is accessible using the
-- Lookup methods.

-- | A ConstantPool is just an 'IntMap'. A 'IntMap' is used, because constants are
-- accessed after their byte-offset. 'constantSize'
newtype ConstantPool = ConstantPool
  { unConstantPool :: IM.IntMap Constant
  } deriving (Show, Eq)

-- -- | Return a list of reference-constant pairs.
-- toListOfConstants :: ConstantPool -> [(, Constant)]
-- toListOfConstants =
--   map (\(a,b) -> (ConstantRef . fromIntegral $ a, b)) . IM.toList . unConstantPool

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
derefConstant :: ConstantPool -> Word16 -> Maybe Constant
derefConstant (ConstantPool cp) ref =
  IM.lookup (fromIntegral ref) cp

-- | A pool access error
data PoolAccessError
  = PoolAccessError Word16 String
  deriving (Show, Eq)

-- | The pool access monad.
newtype PoolAccess a = PoolAccess
  { runWithPool :: ConstantPool -> Either PoolAccessError a
  } deriving (Functor)

instance Applicative (PoolAccess) where
  pure x = PoolAccess . const $ Right x
  mf <*> ma = PoolAccess $ \c ->
     runWithPool mf c <*> runWithPool ma c

instance Monad (PoolAccess) where
  return = pure
  ma >>= fm = PoolAccess $ \c ->
    case runWithPool ma c of
      Left p -> Left p
      Right b -> runWithPool (fm b) c

-- This part contains helpers to describe some of the semantics of the
-- class file.

-- | Describes an index into the constant pool
newtype Index x = Index
  { indexAsWord :: Word16
  } deriving (Show, Eq)

instance (InConstantPool x) => Binary (Index x) where
  get = Index <$> get
  put (Index ref) = put ref

-- | Describes if a type is in the constant pool
class InConstantPool a where
  deref :: Index a -> PoolAccess a

instance InConstantPool Constant where
  deref (Index ref) = PoolAccess $ \cp ->
    case derefConstant cp ref of
      Just c -> Right c
      Nothing -> runWithPool (outofbounds ref) cp

-- | Lookup a 'Text.Text' in the 'ConstantPool', returns 'Nothing' if the
-- reference does not point to something in the ConstantPool, if it points to
-- something not a 'Language.JVM.Constant.String' Constant, or if it is
-- impossible to decode the 'ByteString' as Utf8.
instance InConstantPool Text.Text where
  deref (Index ref) = do
    cons <- derefW ref
    case cons of
      String str ->
        case TE.decodeUtf8' . unSizedByteString $ str of
          Left (TE.DecodeError msg _) -> badEncoding ref msg (unSizedByteString str)
          Left _ -> error "This is deprecated in the api"
          Right txt -> return txt
      a -> wrongType ref "String" a

instance InConstantPool ClassName where
  deref (Index ref) = do
    cons <- derefW ref
    case cons of
      ClassRef r ->
        ClassName <$> deref r
      a -> wrongType ref "ClassRef" a

instance InConstantPool (InClass MethodId) where
  deref (Index ref) = do
    cons <- derefW ref
    case cons of
      MethodRef cn rt ->
        InClass <$> deref cn <*> deref rt
      a -> wrongType ref "MethodRef" a

instance InConstantPool (InClass FieldId) where
  deref (Index ref) = do
    cons <- derefW ref
    case cons of
      FieldRef cn rt ->
        InClass <$> deref cn <*> deref rt
      a -> wrongType ref "FieldRef" a

instance InConstantPool MethodId where
  deref (Index ref) = do
    cons <- derefW ref
    case cons of
      NameAndType n (Index i) ->
        MethodId <$> deref n <*> derefW i
      a -> wrongType ref "NameAndType" a

instance InConstantPool FieldId where
  deref (Index ref) = do
    cons <- derefW ref
    case cons of
      NameAndType n (Index i) ->
        FieldId <$> deref n <*> derefW i
      a -> wrongType ref "NameAndType" a

instance InConstantPool MethodDescriptor where
  deref (Index ref) = do
    txt <- derefW ref
    case methodDescriptorFromText txt of
      Just x -> return x
      Nothing -> parseError ref txt

instance InConstantPool FieldDescriptor where
  deref (Index ref) = do
    txt <- derefW ref
    case fieldDescriptorFromText txt of
      Just x -> return x
      Nothing -> parseError ref txt

-- Helper functions

-- | Given a function from an object b to an 'Index a' create a function from b
-- to 'PoolAccess a'. Very useful for defining functions.
derefF :: InConstantPool a => (b -> Index a) -> b -> PoolAccess a
derefF fn b = deref (fn b)

-- Hidden access method.

derefW :: InConstantPool a => Word16 -> PoolAccess a
derefW w = deref (Index w)

-- Hidden exception methods

wrongType :: Word16 -> String -> Constant -> PoolAccess a
wrongType w expected got = PoolAccess $ \_ ->
  Left (PoolAccessError w ("Expected '" ++ expected ++ "' but got '" ++ typeToStr got ++ "'"))

outofbounds :: Word16 -> PoolAccess a
outofbounds w = PoolAccess $ \_ ->
  Left (PoolAccessError w "Out of bounds")

parseError :: Word16 -> Text.Text -> PoolAccess a
parseError w t = PoolAccess $ \_ ->
  Left (PoolAccessError w $ "Could not parse '" ++ Text.unpack t ++ "'")

badEncoding :: Word16 -> String -> BS.ByteString -> PoolAccess a
badEncoding w str bs = PoolAccess $ \_ ->
  Left (PoolAccessError w $ "Could not encode '" ++ str ++ "': " ++ show bs)
