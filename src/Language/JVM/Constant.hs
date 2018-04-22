{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-|
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'Constant' type and the 'ConstantPool'. These
are essential for accessing data in the class-file.
-}

module Language.JVM.Constant
  ( Constant (..)
  , constantSize
  , typeToStr

  , Referenceable (..)

    -- * Special constants
  , ClassName (..)

  , InClass (..)

  , AbsMethodId
  , AbsFieldId
  , AbsInterfaceMethodId (..)
  , AbsVariableMethodId (..)

  , MethodId (..)
  , methodIdToText
  , FieldId (..)
  , fieldIdToText

  , MethodDescriptor
  , FieldDescriptor


  , MethodHandle (..)
  , MethodHandleField (..)
  , MethodHandleMethod (..)
  , MethodHandleInterface (..)
  , MethodHandleFieldKind (..)
  , InvokeDynamic (..)

  -- * re-exports
  , High
  , Low
  ) where

-- TODO: Data.Binary.IEEE754?

import           Control.DeepSeq          (NFData)
import           Control.Monad.Reader
import           Data.Binary
import           Data.Binary.IEEE754
import           Data.Int
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)
import           Numeric                  (showHex)
import           Prelude                  hiding (fail, lookup)

import           Language.JVM.Stage
import           Language.JVM.TH
import           Language.JVM.Type
import           Language.JVM.Utils

import qualified Data.Text.Encoding.Error as TE

-- import qualified Data.Text as Text
import qualified Data.ByteString          as BS

-- import Data.Functor.Classes


-- | A constant is a multi word item in the 'ConstantPool'. Each of
-- the constructors are pretty much self-explanatory from the types.
data Constant r
  = CString !SizedByteString16
  | CInteger !Int32
  | CFloat !Float
  | CLong !Int64
  | CDouble !Double
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
  , inClassId   :: !(DeepRef a r)
  }

-- | A method identifier
data MethodId r = MethodId
  { methodIdName        :: !(Ref Text.Text r)
  , methodIdDescriptor  :: !(Ref MethodDescriptor r)
  }

-- | A method id in a class.
type AbsMethodId = InClass MethodId

-- | A field identifier
data FieldId r = FieldId
  { fieldIdName        :: !(Ref Text.Text r)
  , fieldIdDescriptor  :: !(Ref FieldDescriptor r)
  } -- deriving (Show, Eq, Ord, Generic, NFData)

-- | A field id in a class
type AbsFieldId = InClass FieldId

-- | An interface method, which is a class in a method.
newtype AbsInterfaceMethodId r = AbsInterfaceMethodId
  { interfaceMethodId :: InClass MethodId r
  }

fieldIdToText :: FieldId High -> Text.Text
fieldIdToText fid =
  Text.concat
  [ value $ fieldIdName fid, ":"
  , fieldDescriptorToText . value $ fieldIdDescriptor fid
  ]

methodIdToText :: MethodId High -> Text.Text
methodIdToText fid =
  Text.concat
  [ value $ methodIdName fid, ":"
  , methodDescriptorToText . value $ methodIdDescriptor fid
  ]

-- | In some cases we can both point to interface methods and
-- regular methods.
data AbsVariableMethodId r
  = VInterfaceMethodId !(AbsInterfaceMethodId r)
  | VMethodId !(AbsMethodId r)

-- | The union type over the different method handles.
data MethodHandle r
  = MHField !(MethodHandleField r)
  | MHMethod !(MethodHandleMethod r)
  | MHInterface !(MethodHandleInterface r)


data MethodHandleField r = MethodHandleField
  { methodHandleFieldKind :: !MethodHandleFieldKind
  , methodHandleFieldRef  :: !(DeepRef AbsFieldId r)
  }

data MethodHandleFieldKind
  = MHGetField
  | MHGetStatic
  | MHPutField
  | MHPutStatic
  deriving (Eq, Show, NFData, Generic, Ord)

data MethodHandleMethod r
  = MHInvokeVirtual !(DeepRef AbsMethodId r)
  | MHInvokeStatic !(DeepRef AbsVariableMethodId r)
  -- ^ Since version 52.0
  | MHInvokeSpecial !(DeepRef AbsVariableMethodId r)
  -- ^ Since version 52.0
  | MHNewInvokeSpecial !(DeepRef AbsMethodId r)

data MethodHandleInterface r = MethodHandleInterface
  {  methodHandleInterfaceRef :: !(DeepRef AbsInterfaceMethodId r)
  }

data InvokeDynamic r = InvokeDynamic
  { invokeDynamicAttrIndex :: !Word16
  , invokeDynamicMethod    :: !(DeepRef MethodId r)
  }


-- | Hack that returns the name of a constant.
typeToStr :: Constant r -> String
typeToStr c =
  case c of
    CString _             -> "CString"
    CInteger _            -> "CInteger"
    CFloat _              -> "CFloat"
    CLong _               -> "CLong"
    CDouble _             -> "CDouble"
    CClassRef _           -> "CClassRef"
    CStringRef _          -> "CStringRef"
    CFieldRef _           -> "CFieldRef"
    CMethodRef _          -> "CMethodRef"
    CInterfaceMethodRef _ -> "CInterfaceMethodRef"
    CNameAndType _ _      -> "CNameAndType"
    CMethodHandle _       -> "CMethodHandle"
    CMethodType _         -> "CMethodType"
    CInvokeDynamic _      -> "CInvokeDynamic"

instance Binary (Constant Low) where
  get = do
    ident <- getWord8
    case ident of
      1  -> CString <$> get
      3  -> CInteger <$> get
      4  -> CFloat <$> getFloat32be
      5  -> CLong <$> get
      6  -> CDouble <$> getFloat64be
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
      CString bs            -> do putWord8 1; put bs
      CInteger i            -> do putWord8 3; put i
      CFloat i              -> do putWord8 4; putFloat32be i
      CLong i               -> do putWord8 5; put i
      CDouble i             -> do putWord8 6; putFloat64be i
      CClassRef i           -> do putWord8 7; put i
      CStringRef i          -> do putWord8 8; put i
      CFieldRef i           -> do putWord8 9; put i
      CMethodRef i          -> do putWord8 10; put i
      CInterfaceMethodRef i -> do putWord8 11; put i
      CNameAndType i j      -> do putWord8 12; put i; put j
      CMethodHandle h       -> do putWord8 15; put h
      CMethodType i         -> do putWord8 16; put i;
      CInvokeDynamic i      -> do putWord8 18; put i

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
      7 -> MHMethod . MHInvokeSpecial<$> get
      8 -> MHMethod . MHNewInvokeSpecial <$> get

      9 -> MHInterface . MethodHandleInterface <$> get

      _ -> fail $ "Unknown method handle kind 'x" ++ showHex w "'"

  put x = case x of
    MHField h -> do
      putWord8 $ case methodHandleFieldKind h of
        MHGetField  -> 1
        MHGetStatic -> 2
        MHPutField  -> 3
        MHPutStatic -> 4
      put $ methodHandleFieldRef h

    MHMethod h -> do
      case h of
        MHInvokeVirtual m    -> putWord8 5 >> put m
        MHInvokeStatic m     -> putWord8 6 >> put m
        MHInvokeSpecial m    -> putWord8 7 >> put m
        MHNewInvokeSpecial m -> putWord8 8 >> put m

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
    _         -> 1

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

-- TODO: Find good encoding of string.
instance Referenceable Text.Text where
  fromConst err c =
    case c of
      CString str ->
        case sizedByteStringToText str of
          Left (TE.DecodeError msg _) ->
            err $ badEncoding msg (unSizedByteString str)
          Left _ -> error "This is deprecated in the api"
          Right txt -> return txt
      a -> err $ wrongType "String" a

  toConst =
    return . CString . sizedByteStringFromText

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
  fromConst err c = expected "CFieldRef" err c

  toConst s =
    return $ CFieldRef s

instance Referenceable (InClass MethodId High) where
  fromConst _ (CMethodRef s) = do
    return $ s
  fromConst err c = expected "CMethodRef" err c

  toConst s =
    return $ CMethodRef s

instance Referenceable (InvokeDynamic High) where
  fromConst _ (CInvokeDynamic c) = do
    return $ c
  fromConst err c = expected "CInvokeDynamic" err c

  toConst s =
    return $ CInvokeDynamic s

instance Referenceable (MethodHandle High) where
  fromConst _ (CMethodHandle c) = do
    return $ c
  fromConst err c = expected "CMethodHandle" err c

  toConst s =
    return $ CMethodHandle s

instance Referenceable (AbsInterfaceMethodId High) where
  fromConst _ (CInterfaceMethodRef s) = do
    return . AbsInterfaceMethodId $ s
  fromConst err c = expected "CInterfaceMethodRef" err c

  toConst (AbsInterfaceMethodId s) =
    return $ CInterfaceMethodRef s

instance Referenceable (AbsVariableMethodId High) where
  fromConst _ (CInterfaceMethodRef s) = do
    return . VInterfaceMethodId . AbsInterfaceMethodId $ s

  fromConst _ (CMethodRef s) = do
    return . VMethodId $ s

  fromConst err c = expected "CInterfaceMethodRef or CMethodRef" err c

  toConst (VInterfaceMethodId (AbsInterfaceMethodId s)) =
    return $ CInterfaceMethodRef s

  toConst (VMethodId s) =
    return $ CMethodRef s

expected :: String -> (String -> a) -> (Constant r) -> a
expected name err c =
  err $ wrongType name c

wrongType :: String -> Constant r -> String
wrongType n c =
  "Expected '" ++ n ++ "', but found '" ++ typeToStr c ++ "'."

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
$(deriveBaseWithBinary ''InvokeDynamic)

$(deriveBaseWithBinary ''AbsMethodId)
$(deriveBaseWithBinary ''AbsFieldId)
$(deriveBaseWithBinary ''AbsInterfaceMethodId)
$(deriveBaseWithBinary ''AbsVariableMethodId)
