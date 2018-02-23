{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'Constant' type and the 'ConstantPool'. These
are essential for accessing data in the class-file.
-}

{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE GADTs    #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.JVM.Constant
  ( Constant (..)
  , constantSize
  , typeToStr

  , Ref (..)
  , DeepRef (..)
  , Choice

  , Index
  , Low
  , High
  , idx
  , valueF
  , value

  , Referenceable (..)

    -- * Special constants
  , ClassName (..)

  , MethodId (..)
  , FieldId (..)
  , InterfaceId (..)

  , MethodDescriptor
  , FieldDescriptor

  , InClass (..)

  , MethodHandle (..)
  , MethodHandleField (..)
  , MethodHandleMethod (..)
  , MethodHandleInterface (..)
  , MethodHandleMethodKind (..)
  , MethodHandleFieldKind (..)
  , InvokeDynamic (..)

  ) where

-- TODO: Data.Binary.IEEE754?

import           Prelude            hiding (fail, lookup)
import           Numeric (showHex)
-- import           Control.Monad.Fail (fail)
import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)
import           Control.Monad.Reader
import           Data.Binary
import qualified Data.Text          as Text

import           Language.JVM.Utils
import           Language.JVM.Type
import           Language.JVM.TH

-- import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

-- import Data.Functor.Classes

data family Ref v r
data instance Ref v High = RefV v
data instance Ref v Low = RefI Word16

deriving instance Show (Ref v Low)
deriving instance NFData (Ref v Low)
deriving instance Generic (Ref v Low)
deriving instance Eq (Ref v Low)

deriving instance Ord (Ref v Low)

deriving instance Show v => Show (Ref v High)
deriving instance NFData v => NFData (Ref v High)
deriving instance Generic (Ref v High)
deriving instance Eq v => Eq (Ref v High)


type family Choice r a b
type instance Choice High a b = b
type instance Choice Low a b = a

type Index = Word16

value :: Ref v High -> v
value (RefV v) = v

valueF :: (a -> Ref v High) -> a -> v
valueF f = value . f


-- data Choice r a b where
--   ChoiceLow :: a -> Choice Low a b
--   ChoiceHigh :: b -> Choice Low a b

instance Binary (Ref a Low) where
  get = RefI <$> get
  put = put . idx

idx :: Ref a Low -> Word16
idx (RefI w) = w

newtype DeepRef v r = DeepRef { unDeep :: (Ref (v r) r) }

deriving instance Show (DeepRef v Low)
deriving instance NFData (DeepRef v Low)
deriving instance Generic (DeepRef v Low)
deriving instance Eq (DeepRef v Low)

deriving instance Show (v High) => Show (DeepRef v High)
deriving instance NFData (v High) => NFData (DeepRef v High)
deriving instance Generic (DeepRef v High)
deriving instance Eq (v High) => Eq (DeepRef v High)

deriving instance Ord (DeepRef v Low)
deriving instance Binary (DeepRef v Low)

-- | A constant is a multi word item in the 'ConstantPool'. Each of
-- the constructors are pretty much self-explanatory from the types.
data Constant r
  = CString !SizedByteString16

  | CInteger !Word32
  | CFloat !Word32
  | CLong !Word64
  | CDouble !Word64
  | CClassRef !(Ref Text.Text r)
  | CStringRef !(Ref Text.Text r)
  | CFieldRef !(InClass FieldId r)
  | CMethodRef !(InClass MethodId r)
  | CInterfaceMethodRef !(InClass MethodId r)
  | CNameAndType !(Ref Text.Text r) !(Ref Text.Text r)
  | CMethodHandle !(MethodHandle r)
  | CMethodType !(Ref MethodDescriptor r)
  | CInvokeDynamic !(InvokeDynamic r)

--deriving (Show, Eq, Generic, NFData)

-- | Anything pointing inside a class
data InClass a r = InClass
  { inClassName :: !(Ref ClassName r)
  , inClassId :: !(DeepRef a r)
  }

-- | A method identifier
data MethodId r = MethodId
  { methodIdName :: !(Ref Text.Text r)
  , methodIdDescription :: !(Ref MethodDescriptor r)
  }

-- | A field identifier
data FieldId r = FieldId
  { fieldIdName :: !(Ref Text.Text r)
  , fieldIdDescription :: !(Ref FieldDescriptor r)
  } -- deriving (Show, Eq, Ord, Generic, NFData)

-- | An interface identifier, essentially a method id
data InterfaceId r = InterfaceId
  { interfaceMethodId :: !(InClass MethodId r)
  }

-- | The union type over the different method handles.
data MethodHandle r
  = MHField !(MethodHandleField r)
  | MHMethod !(MethodHandleMethod r)
  | MHInterface !(MethodHandleInterface r)


data MethodHandleField r = MethodHandleField
  { methodHandleFieldKind :: !MethodHandleFieldKind
  , methodHandleFieldRef :: !(DeepRef (InClass FieldId) r)
  }

data MethodHandleFieldKind
  = MHGetField
  | MHGetStatic
  | MHPutField
  | MHPutStatic
  deriving (Eq, Show, NFData, Generic, Ord)

data MethodHandleMethod r = MethodHandleMethod
  { methodHandleMethodKind :: !MethodHandleMethodKind
  , methodHandleMethodRef :: !(DeepRef (InClass MethodId) r)
  }

data MethodHandleMethodKind
  = MHInvokeVirtual
  | MHInvokeStatic
  | MHInvokeSpecial
  | MHNewInvokeSpecial
  deriving (Eq, Show, NFData, Generic, Ord)

data MethodHandleInterface r = MethodHandleInterface
  {  methodHandleInterfaceRef :: !(DeepRef (InClass MethodId) r)
  }

data InvokeDynamic r = InvokeDynamic
  { invokeDynamicAttrIndex :: !Word16
  , invokeDynamicMethod :: !(DeepRef MethodId r)
  }

-- | Hack that returns the name of a constant.
typeToStr :: Constant r -> String
typeToStr c =
  case c of
    CString _  -> "CString"
    CInteger _  -> "CInteger"
    CFloat _  -> "CFloat"
    CLong _  -> "CLong"
    CDouble _  -> "CDouble"
    CClassRef _  -> "CClassRef"
    CStringRef _  -> "CStringRef"
    CFieldRef _  -> "CFieldRef"
    CMethodRef _  -> "CMethodRef"
    CInterfaceMethodRef _  -> "CInterfaceMethodRef"
    CNameAndType _ _  -> "CNameAndType"
    CMethodHandle _  -> "CMethodHandle"
    CMethodType _  -> "CMethodType"
    CInvokeDynamic _  -> "CInvokeDynamic"

instance Binary (Constant Low) where
  get = do
    ident <- getWord8
    case ident of
      1  -> CString <$> get
      3  -> CInteger <$> get
      4  -> CFloat <$> get
      5  -> CLong <$> get
      6  -> CDouble <$> get
      7  -> CClassRef <$> get
      8  -> CStringRef <$> get
      9  -> CFieldRef <$> get
      10 -> CMethodRef <$> get
      11 -> CInterfaceMethodRef <$> get
      12 -> CNameAndType <$> get <*> get
      15 -> CMethodHandle <$> get
      16 -> CMethodType <$> get
      18 -> CInvokeDynamic <$> get
      _  -> fail $ "Unkown identifier " ++ show ident

  put x =
    case x of
      CString bs              -> do putWord8 1; put bs
      CInteger i              -> do putWord8 3; put i
      CFloat i                -> do putWord8 4; put i
      CLong i                 -> do putWord8 5; put i
      CDouble i               -> do putWord8 6; put i
      CClassRef i             -> do putWord8 7; put i
      CStringRef i            -> do putWord8 8; put i
      CFieldRef i             -> do putWord8 9; put i
      CMethodRef i            -> do putWord8 10; put i
      CInterfaceMethodRef i   -> do putWord8 11; put i
      CNameAndType i j        -> do putWord8 12; put i; put j
      CMethodHandle h         -> do putWord8 15; put h
      CMethodType i           -> do putWord8 16; put i;
      CInvokeDynamic i        -> do putWord8 18; put i

instance Binary (MethodHandle Low) where
  get = do
    w <- getWord8
    case w of
      1 -> MHField . MethodHandleField MHGetField <$> get
      2 -> MHField . MethodHandleField MHGetStatic <$> get
      3 -> MHField . MethodHandleField MHPutField <$> get
      4 -> MHField . MethodHandleField MHPutStatic <$> get

      5 -> MHMethod . MethodHandleMethod MHInvokeVirtual <$> get
      6 -> MHMethod . MethodHandleMethod MHInvokeStatic <$> get
      7 -> MHMethod . MethodHandleMethod MHInvokeSpecial<$> get
      8 -> MHMethod . MethodHandleMethod MHNewInvokeSpecial <$> get

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

    MHMethod h   -> do
      putWord8 $ case methodHandleMethodKind h of
        MHInvokeVirtual-> 5
        MHInvokeStatic -> 6
        MHInvokeSpecial -> 7
        MHNewInvokeSpecial -> 8
      put $ methodHandleMethodRef h

    MHInterface h -> do
      putWord8  9
      put $ methodHandleInterfaceRef h

-- | Some of the 'Constant's take up more space in the constant pool than other.
-- Notice that 'Language.JVM.Constant.String' and 'MethodType' is not of size
-- 32, but is still awarded value 1. This is due to an
-- [inconsistency](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.5)
-- in JVM.
constantSize :: Constant r -> Int
constantSize x =
  case x of
    CDouble _ -> 2
    CLong _   -> 2
    _        -> 1

-- deriving instance NFData i => NFData1 (Index i)
-- deriving instance Generic i => Generic1 (Index i)
-- deriving instance NFData i => NFData1 (Deref i)
-- deriving instance Generic i => Generic1 (Deref i)
-- deriving instance NFData i => NFData1 (Value i)
-- deriving instance Generic i => Generic1 (Value i)

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
  fromConst _ a = return a
  toConst a = return a

instance Referenceable (MethodId High) where
  fromConst err (CNameAndType rn (RefV txt)) = do
    md <- either err return $ methodDescriptorFromText txt
    return $ MethodId rn (RefV md)
  fromConst e c = expected "CNameAndType" e c

  toConst (MethodId rn (RefV md)) =
    return $ CNameAndType rn (RefV $ methodDescriptorToText md)

instance Referenceable (FieldId High) where
  fromConst err (CNameAndType rn (RefV txt)) = do
    md <- either err return $ fieldDescriptorFromText txt
    return $ FieldId rn (RefV md)
  fromConst e c = expected "CNameAndType" e c

  toConst (FieldId rn (RefV md)) =
    return $ CNameAndType rn (RefV $ fieldDescriptorToText md)

instance Referenceable Text.Text where
  fromConst err c =
    case c of
      CString str ->
        case TE.decodeUtf8' . unSizedByteString $ str of
          Left (TE.DecodeError msg _) ->
            err $ badEncoding msg (unSizedByteString str)
          Left _ -> error "This is deprecated in the api"
          Right txt -> return txt
      a -> err $ wrongType "String" a

  toConst txt =
    return $ CString (SizedByteString $ TE.encodeUtf8 txt)

instance Referenceable ClassName where
  fromConst _ (CClassRef (RefV r)) =
    return . ClassName $ r
  fromConst err a =
    err $ wrongType "ClassRef" a

  toConst (ClassName txt) = do
    return . CClassRef $ RefV txt

instance Referenceable MethodDescriptor where
  fromConst err c = do
    txt <- fromConst err c
    either err return $ methodDescriptorFromText txt

  toConst c =
    toConst (methodDescriptorToText c)

instance Referenceable FieldDescriptor where
  fromConst err c = do
    txt <- fromConst err c
    either err return $ fieldDescriptorFromText txt

  toConst c =
    toConst (fieldDescriptorToText c)

instance Referenceable (InClass FieldId High) where
  fromConst _ (CFieldRef s) = do
    return $ s
  fromConst err c = expected "FieldRef" err c

  toConst s =
    return $ CFieldRef s

instance Referenceable (InClass MethodId High) where
  fromConst _ (CMethodRef s) = do
    return $ s
  fromConst err c = expected "MethodRef" err c

  toConst s =
    return $ CMethodRef s


expected :: String -> (String -> a) -> (Constant r) -> a
expected name err c =
  err $ wrongType name c

wrongType :: String -> Constant r -> String
wrongType n c =
  "Expected '" ++ n ++ "', but found'" ++ typeToStr c ++ "'."

badEncoding :: String -> BS.ByteString -> String
badEncoding str bs =
  "Could not encode '" ++ str ++ "': " ++ show bs

$(deriveBase ''Constant)
$(deriveBase ''MethodHandle)
$(deriveBase ''MethodHandleField)
$(deriveBase ''MethodHandleMethod)
$(deriveBase ''MethodHandleInterface)
$(deriveBaseWithBinary ''MethodId)
$(deriveBaseWithBinary ''FieldId)
$(deriveBaseWithBinary ''InterfaceId)
$(deriveBaseWithBinary ''InvokeDynamic)

type AbsMethodId = InClass MethodId
type AbsFieldId = InClass FieldId
$(deriveBaseWithBinary ''AbsMethodId)
$(deriveBaseWithBinary ''AbsFieldId)
