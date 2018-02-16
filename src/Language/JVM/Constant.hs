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
--  , DeepRef 

  , Index (..)
  , idx
  , Deref (..)
  , Value (..)
  , WithValue (..)
  , WithIndex (..)
  , valueF

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

import           Prelude            hiding (fail, lookup)
import           Numeric (showHex)
-- import           Control.Monad.Fail (fail)
import           Control.DeepSeq (NFData, NFData1, rnf, rnf1)
import           GHC.Generics (Generic, Generic1)
import           Control.Monad.Reader
import           Data.Binary
import qualified Data.Text          as Text

import           Language.JVM.Utils
import           Language.JVM.Type
import           Language.JVM.TH

import Data.Functor.Classes (Eq1, Show1, eq1, showsPrec1)

-- | This wraps a reference type 'r' and an 'Referenceable' 'a', and
-- make them an instance of 'Generic', 'Show', 'Eq', and 'NFData'
newtype Ref r a = Ref
  { unref :: (r a)
  }

deriving instance (Generic1 r, Generic a) => Generic (Ref r a)

instance (Show1 r, Show a) => Show (Ref r a) where
  showsPrec i = showsPrec1 i . unref

instance (Eq1 r, Eq a) => Eq (Ref r a) where
  (==) a b = eq1 (unref a) (unref b)

instance (NFData1 r, NFData a) => NFData (Ref r a) where
  rnf = rnf1 . unref

type DeepRef r f = Ref r (f r)

-- | An index into the constant pool
data Index i = Index {-# UNPACK #-} !Word16

instance Binary (Ref Index a) where
  get = Ref . Index <$> get
  put = put . idx

-- | An access into the constant pool, de-referenced.
data Deref i = Deref {-# UNPACK #-} !Word16 !i

newtype Value a = Value a

idx :: Ref Index a -> Word16
idx (Ref (Index w)) = w

class WithValue r where
  getValue :: Ref r v -> v

instance WithValue Value where
  getValue (Ref (Value a)) = a

instance WithValue Deref where
  getValue (Ref (Deref _ a)) = a

class WithIndex r where
  getIndex :: Ref r v -> Word16

instance WithIndex Index where
  getIndex (Ref (Index v)) = v

instance WithIndex Deref where
  getIndex (Ref (Deref v _)) = v

-- refIndex :: Reference r => Ref r a -> Word16
-- refIndex = asWord . unref

valueF :: WithValue r => (b -> Ref r a) -> b -> a
valueF f = getValue . f



-- | A constant is a multi word item in the 'ConstantPool'. Each of
-- the constructors are pretty much self-explanatory from the types.
data Constant r
  = CString !SizedByteString16
  | CInteger !Word32
  | CFloat !Word32
  | CLong !Word64
  | CDouble !Word64
  | CClassRef !(Ref r Text.Text)
  | CStringRef !(Ref r Text.Text)
  | CFieldRef !(InClass FieldId r)
  | CMethodRef !(InClass MethodId r)
  | CInterfaceMethodRef !(InClass MethodId r)
  | CNameAndType !(Ref r Text.Text) !(Ref r Text.Text)
  | CMethodHandle !(MethodHandle r)
  | CMethodType !(Ref r MethodDescriptor)
  | CInvokeDynamic !(InvokeDynamic r)

--deriving (Show, Eq, Generic, NFData)

deriving instance Ord (Constant Index)
deriving instance Eq (Index x)
deriving instance Ord (Index x)
deriving instance Eq (InClass x Index)
deriving instance Ord (InClass x Index)
deriving instance Eq (Ref Index x)
deriving instance Ord (Ref Index x)

-- | Anything pointing inside a class
data InClass a r = InClass
  { inClassName :: !(Ref r ClassName)
  , inClassId :: !(Ref r (a r))
  }

-- | A method identifier
data MethodId r = MethodId
  { methodIdName :: !(Ref r Text.Text)
  , methodIdDescription :: !(Ref r MethodDescriptor)
  }

-- | A field identifier
data FieldId r = FieldId
  { fieldIdName :: !(Ref r Text.Text)
  , fieldIdDescription :: !(Ref r FieldDescriptor)
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

instance Binary (MethodHandle Index) where
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

data MethodHandleField r = MethodHandleField
  { methodHandleFieldKind :: !MethodHandleFieldKind
  , methodHandleFieldRef :: !(DeepRef r (InClass FieldId))
  }

data MethodHandleFieldKind
  = MHGetField
  | MHGetStatic
  | MHPutField
  | MHPutStatic
  deriving (Eq, Show, NFData, Generic, Ord)

data MethodHandleMethod r = MethodHandleMethod
  { methodHandleMethodKind :: !MethodHandleMethodKind
  , methodHandleMethodRef :: !(DeepRef r (InClass MethodId))
  }

data MethodHandleMethodKind
  = MHInvokeVirtual
  | MHInvokeStatic
  | MHInvokeSpecial
  | MHNewInvokeSpecial
  deriving (Eq, Show, NFData, Generic, Ord)

data MethodHandleInterface r = MethodHandleInterface
  {  methodHandleInterfaceRef :: !(DeepRef r (InClass MethodId))
  }

data InvokeDynamic r = InvokeDynamic
  { invokeDynamicAttrIndex :: !Word16
  , invokeDynamicMethod :: !(DeepRef r MethodId)
  }

-- | Hack that returns the name of a constant.
typeToStr :: Show1 r => Constant r -> String
typeToStr = head . words . show

instance Binary (Constant Index) where
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

$(deriveShow1 ''Index)
$(deriveShow1 ''Deref)
$(deriveShow1 ''Value)
$(deriveEq1 ''Index)
$(deriveEq1 ''Deref)
$(deriveEq1 ''Value)

deriving instance NFData1 Index
deriving instance Generic1 Index
deriving instance NFData1 Deref
deriving instance Generic1 Deref
deriving instance NFData1 Value
deriving instance Generic1 Value

$(deriveBase ''Constant)
$(deriveBaseBO ''Index ''MethodId)
$(deriveBaseBO ''Index ''FieldId)
$(deriveBaseBO ''Index ''InterfaceId)
$(deriveBaseO ''Index ''MethodHandle)
$(deriveBaseO ''Index ''MethodHandleField)
$(deriveBaseO ''Index ''MethodHandleMethod)
$(deriveBaseO ''Index ''MethodHandleInterface)
$(deriveBaseBO ''Index ''InvokeDynamic)

type AbsMethodId = InClass MethodId
type AbsFieldId = InClass FieldId
$(deriveBaseBO ''Index ''AbsMethodId)
$(deriveBaseBO ''Index ''AbsFieldId)
