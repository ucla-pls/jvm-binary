{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'Constant' type and the 'ConstantPool'. These
are essential for accessing data in the class-file.
-}
module Language.JVM.Constant (
  Constant (..),
  constantSize,
  typeToStr,
  Referenceable (..),

  -- * JValue
  JValue (..),
  VInteger,
  VLong,
  VDouble,
  VFloat,
  VString,

  -- * Special constants
  ClassName (..),
  InClass (..),
  InRefType (..),
  parseAbsMethodId,
  AbsFieldId (..),
  AbsInterfaceMethodId (..),
  AbsVariableMethodId (..),
  MethodId (..),
  FieldId (..),
  NameAndType (..),
  MethodDescriptor,
  FieldDescriptor,
  MethodHandle (..),
  MethodHandleField (..),
  MethodHandleMethod (..),
  MethodHandleInterface (..),
  MethodHandleFieldKind (..),
  InvokeDynamic (..),

  -- * re-exports
  High,
  Low,
)
where

import Control.DeepSeq (NFData)
import Control.Monad.Reader
import Data.Binary
import Data.Binary.IEEE754
import Control.Monad

import qualified Data.ByteString as BS
import Data.Int
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding.Error as TE
import GHC.Generics (Generic)
import Numeric (showHex)
import Prelude hiding (
  fail,
  lookup,
 )

import Language.JVM.Stage
import Language.JVM.TH
import Language.JVM.Type
import Language.JVM.Utils

{- | A constant is a multi word item in the 'ConstantPool'. Each of
 the constructors are pretty much self-explanatory from the types.
-}
data Constant r
  = CString !SizedByteString16
  | CInteger !Int32
  | CFloat !Float
  | CLong !Int64
  | CDouble !Double
  | CClassRef !(Ref Text.Text r)
  | CStringRef !(Ref BS.ByteString r)
  | CFieldRef !(Choice (Index, Index) AbsFieldId r)
  | CMethodRef !(Choice (Index, Index) (InRefType MethodId) r)
  | CInterfaceMethodRef !(Choice (Index, Index) (InRefType MethodId) r)
  | CNameAndType !(Ref Text.Text r) !(Ref Text.Text r)
  | CMethodHandle !(MethodHandle r)
  | CMethodType !(Ref MethodDescriptor r)
  | CInvokeDynamic !(InvokeDynamic r)

-- | An method which is from an interface
newtype AbsInterfaceMethodId = AbsInterfaceMethodId
  { interfaceMethodId :: InRefType MethodId
  }
  deriving (Show, Eq, Generic, NFData)

-- | An method which can be from an interface
data AbsVariableMethodId = AbsVariableMethodId
  { variableIsInterface :: !Bool
  , variableMethodId :: !(InRefType MethodId)
  }
  deriving (Show, Eq, Generic, NFData)

-- | The union type over the different method handles.
data MethodHandle r
  = MHField !(MethodHandleField r)
  | MHMethod !(MethodHandleMethod r)
  | MHInterface !(MethodHandleInterface r)

data MethodHandleField r = MethodHandleField
  { methodHandleFieldKind :: !MethodHandleFieldKind
  , methodHandleFieldRef :: !(Ref AbsFieldId r)
  }

data MethodHandleFieldKind
  = MHGetField
  | MHGetStatic
  | MHPutField
  | MHPutStatic
  deriving (Eq, Show, NFData, Generic, Ord)

data MethodHandleMethod r
  = MHInvokeVirtual !(Ref (InRefType MethodId) r)
  | -- | Since version 52.0
    MHInvokeStatic !(Ref AbsVariableMethodId r)
  | -- | Since version 52.0
    MHInvokeSpecial !(Ref AbsVariableMethodId r)
  | MHNewInvokeSpecial !(Ref (InRefType MethodId) r)

newtype MethodHandleInterface r = MethodHandleInterface
  { methodHandleInterfaceRef :: Ref AbsInterfaceMethodId r
  }

data InvokeDynamic r = InvokeDynamic
  { invokeDynamicAttrIndex :: !Word16
  , invokeDynamicMethod :: !(Ref MethodId r)
  }

-- | Hack that returns the name of a constant.
typeToStr :: Constant r -> String
typeToStr c = case c of
  CString _ -> "CString"
  CInteger _ -> "CInteger"
  CFloat _ -> "CFloat"
  CLong _ -> "CLong"
  CDouble _ -> "CDouble"
  CClassRef _ -> "CClassRef"
  CStringRef _ -> "CStringRef"
  CFieldRef _ -> "CFieldRef"
  CMethodRef _ -> "CMethodRef"
  CInterfaceMethodRef _ -> "CInterfaceMethodRef"
  CNameAndType _ _ -> "CNameAndType"
  CMethodHandle _ -> "CMethodHandle"
  CMethodType _ -> "CMethodType"
  CInvokeDynamic _ -> "CInvokeDynamic"

instance Binary (MethodHandle Low) where
  get = do
    w <- getWord8
    case w of
      1 -> MHField . MethodHandleField MHGetField <$> get
      2 -> MHField . MethodHandleField MHGetStatic <$> get
      3 -> MHField . MethodHandleField MHPutField <$> get
      4 -> MHField . MethodHandleField MHPutStatic <$> get
      5 -> MHMethod . MHInvokeVirtual <$> get
      6 -> MHMethod . MHInvokeStatic <$> get
      7 -> MHMethod . MHInvokeSpecial <$> get
      8 -> MHMethod . MHNewInvokeSpecial <$> get
      9 -> MHInterface . MethodHandleInterface <$> get
      _ -> fail $ "Unknown method handle kind 'x" ++ showHex w "'"

  put x = case x of
    MHField h -> do
      putWord8 $ case methodHandleFieldKind h of
        MHGetField -> 1
        MHGetStatic -> 2
        MHPutField -> 3
        MHPutStatic -> 4
      put $ methodHandleFieldRef h
    MHMethod h -> case h of
      MHInvokeVirtual m -> putWord8 5 >> put m
      MHInvokeStatic m -> putWord8 6 >> put m
      MHInvokeSpecial m -> putWord8 7 >> put m
      MHNewInvokeSpecial m -> putWord8 8 >> put m
    MHInterface h -> do
      putWord8 9
      put $ methodHandleInterfaceRef h

{- | Some of the 'Constant's take up more space in the constant pool than other.
 Notice that 'Language.JVM.Constant.String' and 'MethodType' is not of size
 32, but is still awarded value 1. This is due to an
 [inconsistency](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.5)
 in JVM.
-}
constantSize :: Constant r -> Index
constantSize x = case x of
  CDouble _ -> 2
  CLong _ -> 2
  _ -> 1

-- | 'Referenceable' is something that can exist in the constant pool.
class Referenceable a where
  fromConst
    :: (Monad m)
    => (forall a'. String -> m a')
    -> Constant High
    -> m a
  toConst
    :: (Monad m)
    => a
    -> m (Constant High)

instance Referenceable (Constant High) where
  fromConst _ = return
  toConst = return

instance TextSerializable a => Referenceable (NameAndType a) where
  fromConst err (CNameAndType rn txt) = do
    md <- either err return $ deserialize txt
    return $ NameAndType rn md
  fromConst e c = expected "CNameAndType" e c

  toConst (NameAndType rn md) = return $ CNameAndType rn (serialize md)

-- TODO: Find good encoding of string.
instance Referenceable Text.Text where
  fromConst err c = case c of
    CString str -> case sizedByteStringToText str of
      Left (TE.DecodeError msg _) ->
        err $ badEncoding msg (unSizedByteString str)
      Left _ -> error "This is deprecated in the api"
      Right txt -> return txt
    a -> err $ wrongType "String" a

  toConst = return . CString . sizedByteStringFromText

instance Referenceable BS.ByteString where
  fromConst err c = case c of
    CString str -> return $ unSizedByteString str
    a -> err $ wrongType "String" a
  toConst = return . CString . SizedByteString

instance Referenceable ClassName where
  fromConst err = \case
    CClassRef r -> case textCls r of
      Right cn -> return cn
      Left msg ->
        err $ "Could not read class name: " <> Text.unpack r <> ": " <> msg
    a -> err $ wrongType "ClassRef" a

  toConst (classNameAsText -> txt) = return . CClassRef $ txt

instance Referenceable JRefType where
  fromConst err = \case
    CClassRef r -> case deserializeWith parseFlatJRefType r of
      Right t -> return t
      Left msg ->
        err $
          "Could not read the flat reference type: "
            <> Text.unpack r
            <> ": "
            <> msg
    a -> err $ wrongType "ClassRef" a
  toConst = return . CClassRef . serializeWith serializeFlatJRefType

instance Referenceable ReturnDescriptor where
  fromConst err = fromConst err >=> either err return . deserialize
  toConst = toConst . serialize

instance Referenceable MethodDescriptor where
  fromConst err = fromConst err >=> either err pure . deserialize
  toConst = toConst . serialize

instance Referenceable FieldDescriptor where
  fromConst err = fromConst err >=> either err pure . deserialize
  toConst = toConst . serialize

instance Referenceable MethodId where
  fromConst err x = MethodId <$> fromConst err x
  toConst (MethodId s) = toConst s

instance Referenceable FieldId where
  fromConst err x = FieldId <$> fromConst err x
  toConst (FieldId s) = toConst s

instance Referenceable AbsFieldId where
  fromConst err = \case
    CFieldRef s -> return s
    c -> expected "CFieldRef" err c

  toConst s = return $ CFieldRef s

instance Referenceable (InRefType MethodId) where
  fromConst err = \case
    CMethodRef s -> return $ s
    c -> expected "CMethodRef" err c

  toConst s = return $ CMethodRef s

instance Referenceable AbsVariableMethodId where
  fromConst err = \case
    CMethodRef s -> return $ AbsVariableMethodId False s
    CInterfaceMethodRef s -> return $ AbsVariableMethodId True s
    c -> expected "CMethodRef or CInterfaceMethodRef" err c

  toConst (AbsVariableMethodId t s) =
    return $ if t then CInterfaceMethodRef s else CMethodRef s

instance Referenceable AbsInterfaceMethodId where
  fromConst _ (CInterfaceMethodRef s) = return . AbsInterfaceMethodId $ s
  fromConst err c = expected "CInterfaceMethodRef" err c

  toConst (AbsInterfaceMethodId s) = return $ CInterfaceMethodRef s

instance Referenceable (InvokeDynamic High) where
  fromConst _ (CInvokeDynamic c) = return c
  fromConst err c = expected "CInvokeDynamic" err c

  toConst s = return $ CInvokeDynamic s

instance Referenceable (MethodHandle High) where
  fromConst _ (CMethodHandle c) = return c
  fromConst err c = expected "CMethodHandle" err c

  toConst s = return $ CMethodHandle s

expected :: String -> (String -> a) -> (Constant r) -> a
expected name err c = err $ wrongType name c

wrongType :: String -> Constant r -> String
wrongType n c = "Expected '" ++ n ++ "', but found '" ++ typeToStr c ++ "'."

badEncoding :: String -> BS.ByteString -> String
badEncoding str bs = "Could not encode '" ++ str ++ "': " ++ show bs

$( deriveAll
    [
      (
        [ ''MethodHandle
        , ''MethodHandleField
        , ''MethodHandleMethod
        , ''MethodHandleInterface
        , ''Constant
        ]
      , bases
      )
    ,
      (
        [ ''InvokeDynamic
        ]
      , bases ++ [binary]
      )
    ]
 )

type VInteger = Int32
type VLong = Int64
type VFloat = Float
type VDouble = Double
type VString = BS.ByteString

instance Referenceable VInteger where
  fromConst err = \case
    CInteger i -> return i
    x -> expected "Integer" err x
  toConst = return . CInteger

instance Referenceable VLong where
  fromConst err = \case
    CLong i -> return i
    x -> expected "Long" err x
  toConst = return . CLong

instance Referenceable VFloat where
  fromConst err = \case
    CFloat i -> return i
    x -> expected "Float" err x
  toConst = return . CFloat

instance Referenceable VDouble where
  fromConst err = \case
    CDouble i -> return i
    x -> expected "Double" err x
  toConst = return . CDouble

-- | A constant pool value in java
data JValue
  = VInteger VInteger
  | VLong VLong
  | VFloat VFloat
  | VDouble VDouble
  | VString VString
  | VClass JRefType
  | VMethodType MethodDescriptor
  | VMethodHandle (MethodHandle High)
  deriving (Show, Eq, Generic, NFData)

instance Referenceable JValue where
  fromConst err = \case
    CStringRef s -> return $ VString s
    CInteger i -> return $ VInteger i
    CFloat f -> return $ VFloat f
    CLong l -> return $ VLong l
    CDouble d -> return $ VDouble d
    CClassRef r -> case deserializeWith parseFlatJRefType r of
      Right rt -> return $ VClass rt
      Left msg ->
        err $ "Could not parse reftype " <> Text.unpack r <> ": " <> msg
    CMethodHandle m -> return $ VMethodHandle m
    CMethodType t -> return $ VMethodType t
    x -> expected "Value" err x
  {-# INLINE fromConst #-}

  toConst =
    return . \case
      VString s -> CStringRef s
      VInteger i -> CInteger i
      VFloat f -> CFloat f
      VLong l -> CLong l
      VDouble d -> CDouble d
      VClass (serializeWith serializeFlatJRefType -> r) -> CClassRef r
      VMethodHandle m -> CMethodHandle m
      VMethodType t -> CMethodType t
  {-# INLINE toConst #-}

instance Binary (Constant Low) where
  get = do
    ident <- getWord8
    case ident of
      1 -> CString <$> get
      3 -> CInteger <$> get
      4 -> CFloat <$> getFloat32be
      5 -> CLong <$> get
      6 -> CDouble <$> getFloat64be
      7 -> CClassRef <$> get
      8 -> CStringRef <$> get
      9 -> CFieldRef <$> get
      10 -> CMethodRef <$> get
      11 -> CInterfaceMethodRef <$> get
      12 -> CNameAndType <$> get <*> get
      15 -> CMethodHandle <$> get
      16 -> CMethodType <$> get
      18 -> CInvokeDynamic <$> get
      _ -> fail $ "Unknown identifier " ++ show ident

  put x = case x of
    CString bs -> do
      putWord8 1
      put bs
    CInteger i -> do
      putWord8 3
      put i
    CFloat i -> do
      putWord8 4
      putFloat32be i
    CLong i -> do
      putWord8 5
      put i
    CDouble i -> do
      putWord8 6
      putFloat64be i
    CClassRef i -> do
      putWord8 7
      put i
    CStringRef i -> do
      putWord8 8
      put i
    CFieldRef i -> do
      putWord8 9
      put i
    CMethodRef i -> do
      putWord8 10
      put i
    CInterfaceMethodRef i -> do
      putWord8 11
      put i
    CNameAndType i j -> do
      putWord8 12
      put i
      put j
    CMethodHandle h -> do
      putWord8 15
      put h
    CMethodType i -> do
      putWord8 16
      put i
    CInvokeDynamic i -> do
      putWord8 18
      put i
