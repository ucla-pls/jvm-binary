{-# LANGUAGE UndecidableInstances #-}
{-|
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'Constant' type and the 'ConstantPool'. These
are essential for accessing data in the class-file.
-}

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
  (
    -- * Constant Pool
    -- $ConstantPool
    ConstantPool (..)

    -- ** Reference
  , Reference (..)
  , Ref (..)
  , Index (..)
  , Deref (..)

  , refValue
  , refIndex
  , valueF

  -- ** Dereferencing
  , PoolAccessError(..)
  , derefConstant

  , Referenceable (..)

  , deref
  , ClassFileReadable (..)

    -- * Constants
  , Constant (..)
  , constantSize
  , typeToStr

    -- ** Special constants
  , ClassName (..)

  , MethodId (..)
  , FieldId (..)
  , InterfaceId (..)

  , MethodDescriptor
  , FieldDescriptor

  , InClass (..)
  , AbsFieldId
  , AbsMethodId

  , MethodHandle (..)
  , MethodHandleField (..)
  , MethodHandleMethod (..)
  , MethodHandleInterface (..)
  , MethodHandleMethodKind (..)
  , MethodHandleFieldKind (..)
  , InvokeDynamic (..)
  ) where

import           Prelude            hiding (fail, lookup)

import Numeric (showHex)

import           Control.Monad      (forM_)
import           Control.Monad.Fail (fail)
import           Control.DeepSeq (NFData, NFData1, rnf, rnf1)

import           GHC.Generics (Generic, Generic1)

import           Data.Binary
import           Data.Monoid
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.IntMap.Strict as IM

import           Language.JVM.Utils
import           Language.JVM.Type

import           Text.Show.Deriving
import           Data.Eq.Deriving

import qualified Data.Text          as Text
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import Data.Functor.Classes (Eq1, Show1, eq1, showsPrec1)

-- | A ConstantPool is just an 'IntMap'. A 'IntMap' is used, because constants are
-- accessed after their byte-offset. 'constantSize'
newtype ConstantPool r = ConstantPool
  { unConstantPool :: IM.IntMap (Constant r)
  }

deriving instance Reference r => Show (ConstantPool r)
deriving instance Reference r => Eq (ConstantPool r)
deriving instance Reference r => Generic (ConstantPool r)
deriving instance Reference r => NFData (ConstantPool r)

instance Binary (ConstantPool Index) where
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

-- | A reference into the 'ConstantPool', has a single function that
-- returns the index value as a 'Word16'. There are two implementations
-- of this class 'Index' which only saves the index into the ConstantPool
-- and 'Deref' that also stores the value.
class (Generic1 r, Eq1 r, Show1 r, NFData1 r) => Reference r where
  asWord :: r a -> Word16

-- | This wraps a 'Reference' 'r' and an 'Referenceable' 'a', and
-- make them an instance of 'Generic', 'Show', 'Eq', and 'NFData'
newtype Ref r a = Ref { unref :: (r a) }

instance (Reference r, Show a) => Show (Ref r a) where
  showsPrec i = showsPrec1 i . unref

instance (Reference r, Eq a) => Eq (Ref r a) where
  (==) a b = eq1 (unref a) (unref b)

instance (Reference r, NFData a) => NFData (Ref r a) where
  rnf = rnf1 . unref

deriving instance (Reference r, Generic a) => Generic (Ref r a)

-- | An index into the constant pool
data Index i = Index
  { indexAsWord :: {-# UNPACK #-} !Word16
  } -- deriving (Show, Eq, Generic, NFData, Binary)

instance Binary (Ref Index a) where
  get = get
  put a =
    put . indexAsWord . unref $ a

deriving instance NFData1 Index
deriving instance Generic1 Index

instance Reference Index where
  asWord = indexAsWord

unsafeCast :: Ref Index a -> Ref Index b
unsafeCast (Ref (Index x)) =
  Ref (Index x)

-- | An access into the constant pool, de-referenced.
data Deref i = Deref
  { derefIndex :: {-# UNPACK #-} !Word16
  , derefValue :: i
  } -- deriving (Show, Eq, Generic, NFData)

deriving instance (NFData1 Deref)
deriving instance (Generic1 Deref)

refValue :: Ref Deref a -> a
refValue = derefValue . unref

refIndex :: Reference r => Ref r a -> Word16
refIndex = asWord . unref

valueF :: (b -> Ref Deref a) -> b -> a
valueF f = refValue . f

instance Reference Deref where
  asWord = derefIndex

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
  | CFieldRef !(AbsFieldId r)
  | CMethodRef !(AbsMethodId r)
  | CInterfaceMethodRef !(AbsMethodId r)
  | CNameAndType !(Ref r Text.Text) !(Ref r Text.Text)
  | CMethodHandle !(MethodHandle r)
  | CMethodType !(Ref r MethodDescriptor)
  | CInvokeDynamic !(InvokeDynamic r)

deriving instance Reference r => Show (Constant r)
deriving instance Reference r => Eq (Constant r)
deriving instance Reference r => Generic (Constant r)
deriving instance Reference r => NFData (Constant r)

--deriving (Show, Eq, Generic, NFData)

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

deriving instance Reference r => Show (MethodId r)
deriving instance Reference r => Eq (MethodId r)
deriving instance Reference r => Generic (MethodId r)
deriving instance Reference r => NFData (MethodId r)
deriving instance Binary (MethodId Index)

-- | An absolute reference to a method
type AbsMethodId = InClass MethodId

deriving instance Reference r => Show (AbsMethodId r)
deriving instance Reference r => Eq (AbsMethodId r)
deriving instance Reference r => Generic (AbsMethodId r)
deriving instance Reference r => NFData (AbsMethodId r)
deriving instance Binary (AbsMethodId Index)

-- | A field identifier
data FieldId r = FieldId
  { fieldIdName :: !(Ref r Text.Text)
  , fieldIdDescription :: !(Ref r FieldDescriptor)
  } -- deriving (Show, Eq, Ord, Generic, NFData)

deriving instance Reference r => Show (FieldId r)
deriving instance Reference r => Eq (FieldId r)
deriving instance Reference r => Generic (FieldId r)
deriving instance Reference r => NFData (FieldId r)
deriving instance Binary (FieldId Index)

-- | An absolute reference to a field
type AbsFieldId = InClass FieldId

deriving instance Reference r => Show (AbsFieldId r)
deriving instance Reference r => Eq (AbsFieldId r)
deriving instance Reference r => Generic (AbsFieldId r)
deriving instance Reference r => NFData (AbsFieldId r)
deriving instance Binary (AbsFieldId Index)


-- | An interface identifier, essentially a method id
data InterfaceId r = InterfaceId
  { interfaceMethodId :: !(AbsMethodId r)
  }

deriving instance Reference r => Show (InterfaceId r)
deriving instance Reference r => Eq (InterfaceId r)
deriving instance Reference r => Generic (InterfaceId r)
deriving instance Reference r => NFData (InterfaceId r)

-- | The union type over the different method handles.
data MethodHandle r
  = MHField !(MethodHandleField r)
  | MHMethod !(MethodHandleMethod r)
  | MHInterface !(MethodHandleInterface r)

deriving instance Reference r => Show (MethodHandle r)
deriving instance Reference r => Eq (MethodHandle r)
deriving instance Reference r => Generic (MethodHandle r)
deriving instance Reference r => NFData (MethodHandle r)

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
        MHNewInvokeSpecial -> 7
      put $ methodHandleMethodRef h

    MHInterface h -> do
      putWord8  9
      put $ methodHandleInterfaceRef h


data MethodHandleField r = MethodHandleField
  { methodHandleFieldKind :: !MethodHandleFieldKind
  , methodHandleFieldRef :: !(Ref r (AbsFieldId r))
  }

deriving instance Reference r => Show (MethodHandleField r)
deriving instance Reference r => Eq (MethodHandleField r)
deriving instance Reference r => Generic (MethodHandleField r)
deriving instance Reference r => NFData (MethodHandleField r)

data MethodHandleFieldKind
  = MHGetField
  | MHGetStatic
  | MHPutField
  | MHPutStatic
  deriving (Eq, Show, NFData, Generic)

data MethodHandleMethod r = MethodHandleMethod
  { methodHandleMethodKind :: !MethodHandleMethodKind
  , methodHandleMethodRef :: !(Ref r (AbsMethodId r))
  }

deriving instance Reference r => Show (MethodHandleMethod r)
deriving instance Reference r => Eq (MethodHandleMethod r)
deriving instance Reference r => Generic (MethodHandleMethod r)
deriving instance Reference r => NFData (MethodHandleMethod r)

data MethodHandleMethodKind
  = MHInvokeVirtual
  | MHInvokeStatic
  | MHInvokeSpecial
  | MHNewInvokeSpecial
  deriving (Eq, Show, NFData, Generic)

data MethodHandleInterface r = MethodHandleInterface
  {  methodHandleInterfaceRef :: !(Ref r (AbsMethodId r))
  }
deriving instance Reference r => Show (MethodHandleInterface r)
deriving instance Reference r => Eq (MethodHandleInterface r)
deriving instance Reference r => Generic (MethodHandleInterface r)
deriving instance Reference r => NFData (MethodHandleInterface r)

data InvokeDynamic r = InvokeDynamic
  { invokeDynamicAttrIndex :: !Word16
  , invokeDynamicMethod :: !(Ref r (MethodId r))
  }

deriving instance Reference r => Show (InvokeDynamic r)
deriving instance Reference r => Eq (InvokeDynamic r)
deriving instance Reference r => Generic (InvokeDynamic r)
deriving instance Reference r => NFData (InvokeDynamic r)
deriving instance Binary (InvokeDynamic Index)

-- | Hack that returns the name of a constant.
typeToStr :: Reference r => Constant r -> String
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
constantSize :: Reference r => Constant r -> Int
constantSize x =
  case x of
    CDouble _ -> 2
    CLong _   -> 2
    _        -> 1

-- $ConstantPool
-- The 'ConstantPool' contains all the constants, and is accessible using the
-- Lookup methods.

wrongType :: Reference r => String -> Constant r -> String
wrongType n cons =
  "Expected '" ++ n ++ "', but found'" ++ typeToStr cons ++ "'."

badEncoding :: String -> BS.ByteString -> String
badEncoding str bs =
  "Could not encode '" ++ str ++ "': " ++ show bs

-- | 'Referenceable' is something that can exist in the constant pool.
class Referenceable a where
  fromConst :: Constant Deref -> Either String a

instance Referenceable Text.Text where
  fromConst cons =
    case cons of
      CString str ->
        case TE.decodeUtf8' . unSizedByteString $ str of
          Left (TE.DecodeError msg _) -> Left $ badEncoding msg (unSizedByteString str)
          Left _ -> error "This is deprecated in the api"
          Right txt -> return txt
      a -> Left $ wrongType "String" a

instance Referenceable ClassName where
  fromConst (CClassRef (Ref r)) =
    return . ClassName $ derefValue r
  fromConst a = Left $ wrongType "ClassRef" a

instance Referenceable FieldDescriptor where
  fromConst c = do
    txt <- fromConst c
    fieldDescriptorFromText txt

instance Referenceable (FieldId Deref) where
  fromConst (CNameAndType rn (Ref (Deref w txt))) = do
    t' <- fieldDescriptorFromText $ txt
    return $ FieldId rn (Ref (Deref w t'))
  fromConst c = Left $ wrongType "NameAndType" c

instance Referenceable MethodDescriptor where
  fromConst c = do
    txt <- fromConst c
    methodDescriptorFromText txt

instance Referenceable (MethodId Deref) where
  fromConst (CNameAndType rn (Ref (Deref w txt))) = do
    t' <- methodDescriptorFromText $ txt
    return $ MethodId rn (Ref (Deref w t'))
  fromConst c = Left $ wrongType "NameAndType" c

instance Referenceable (InClass FieldId Deref) where
  fromConst (CFieldRef c) = do
    return $ c
  fromConst c = Left $ wrongType "FieldRef" c

instance Referenceable (InClass MethodId Deref) where
  fromConst (CMethodRef c) = do
    return $ c
  fromConst c = Left $ wrongType "FieldRef" c


-- | A pool access error
data PoolAccessError = PoolAccessError
  { paErrorRef :: !Word16
  , paErrorMsg :: String
  } deriving (Show, Eq, Generic, NFData)

-- | Lookup a 'Constant' in the 'ConstantPool'.
derefConstant :: Ref Index i -> ConstantPool Deref -> Either PoolAccessError (Constant Deref)
derefConstant (Ref (Index ref)) (ConstantPool cp) =
  case IM.lookup (fromIntegral ref) cp of
    Just x -> Right x
    Nothing -> Left $ PoolAccessError ref "No such element."

deref :: Referenceable a => Ref Index a -> ConstantPool Deref -> Either ClassFileError (Ref Deref a)
deref r cp =
  case derefConstant r cp of
    Left msg -> Left $ CFEPoolAccessError msg
    Right c -> do
      case fromConst c of
        Left str -> Left (CFEConversionError str)
        Right a -> Right $ Ref (Deref (indexAsWord . unref $ r) a)

data ClassFileError
  = CFEPoolAccessError !PoolAccessError
  | CFEInconsistentClassPool !String
  | CFEConversionError !String
  deriving (Show, Eq, Generic, NFData)

class ClassFileReadable f where
  untie :: f Index -> ConstantPool Deref -> Either ClassFileError (f Deref)

-- | Untie the constant pool, this requires a special operation as the constant pool
-- might reference itself.
bootstrapUntie :: ConstantPool Index -> Either ClassFileError (ConstantPool Deref)
bootstrapUntie reffed =
  case stage (ConstantPool IM.empty, IM.toList $ unConstantPool reffed) of
    (cp, []) ->
      Right cp
    (_, _:_) ->
      Left (CFEInconsistentClassPool "Could not load all constants in the constant pool")
  where
    stage (cp, mis) =
      if IM.null cp'
      then
        (cp, mis)
      else
        stage (ConstantPool (unConstantPool cp `IM.union` cp'), appEndo mis' [])
      where (cp', mis') = foldMap (grow cp) mis

    grow cp (k,a) =
      case untie a cp of
        Right c -> (IM.singleton k c, Endo id)
        Left _ -> (IM.empty, Endo ((k,a):))

instance ClassFileReadable ConstantPool where
  untie cp = const $ bootstrapUntie cp

instance (Referenceable (a Deref)) => ClassFileReadable (InClass a) where
  untie (InClass cn cid) cp = do
    cn' <- deref cn cp
    cid' <- deref (unsafeCast cid) cp
    return $ InClass cn' cid'

instance ClassFileReadable MethodHandle where
  untie (MHField r) cp =
    MHField <$> untie r cp
  untie (MHMethod r) cp =
    MHMethod <$> untie r cp
  untie (MHInterface r) cp =
    MHInterface <$> untie r cp

instance ClassFileReadable MethodHandleField where
  untie (MethodHandleField k ref) cp = do
    MethodHandleField k <$> deref (unsafeCast ref) cp

instance ClassFileReadable MethodHandleMethod where
  untie (MethodHandleMethod k ref) cp =
    MethodHandleMethod k <$> deref (unsafeCast ref) cp

instance ClassFileReadable MethodHandleInterface where
  untie (MethodHandleInterface ref) cp =
    MethodHandleInterface <$> deref (unsafeCast ref) cp

instance ClassFileReadable InvokeDynamic where
  untie (InvokeDynamic w ref) cp =
    InvokeDynamic w <$> deref (unsafeCast ref) cp

instance ClassFileReadable Constant where
  untie c cp =
    case c of
      CString st -> return $ CString st
      CInteger i -> return $ CInteger i
      CFloat f -> return $ CFloat f
      CLong l -> return $ CLong l
      CDouble l -> return $ CDouble l
      CClassRef r ->
        CClassRef <$> deref r cp
      CStringRef r ->
        CStringRef <$> deref r cp
      CFieldRef a ->
        CFieldRef <$> untie a cp
      CMethodRef m ->
        CMethodRef <$> untie m cp
      CInterfaceMethodRef i ->
        CInterfaceMethodRef <$> untie i cp
      CNameAndType r1 r2-> do
        t1 <- deref r1 cp
        t2 <- deref r2 cp
        return $ CNameAndType t1 t2
      CMethodHandle r1 -> do
        CMethodHandle <$> untie r1 cp
      CMethodType m -> do
        CMethodType <$> deref m cp
      CInvokeDynamic t -> do
        t' <- untie t cp
        return $ CInvokeDynamic t'

-- -- | The pool access monad.
-- newtype PoolAccess a = PoolAccess
--   { runWithPool :: (ConstantPool Ref) -> Either PoolAccessError a
--   } deriving (Functor)

-- instance Applicative (PoolAccess) where
--   pure x = PoolAccess . const $ Right x
--   mf <*> ma = PoolAccess $ \c ->
--      runWithPool mf c <*> runWithPool ma c

-- instance Monad (PoolAccess) where
--   return = pure
--   ma >>= fm = PoolAccess $ \c ->
--     case runWithPool ma c of
--       Left p -> Left p
--       Right b -> runWithPool (fm b) c

-- -- This part contains helpers to describe some of the semantics of the
-- -- class file.

-- -- | Describes an index into the constant pool
-- newtype Index x = Index
--   { indexAsWord :: Word16
--   } deriving (Show, Eq, Generic, NFData)

-- instance (InConstantPool x) => Binary (Index x) where
--   get = Index <$> get
--   put (Index ref) = put ref

-- -- | Describes if a type is in the constant pool
-- class InConstantPool a where
--   deref :: Index a -> PoolAccess a

-- instance InConstantPool Constant where
--   deref (Index ref) = PoolAccess $ \cp ->
--     case derefConstant cp ref of
--       Just c -> Right c
--       Nothing -> runWithPool (outofbounds ref) cp

-- -- | Lookup a 'Text.Text' in the 'ConstantPool', returns 'Nothing' if the
-- -- reference does not point to something in the ConstantPool, if it points to
-- -- something not a 'Language.JVM.Constant.String' Constant, or if it is
-- -- impossible to decode the 'ByteString' as Utf8.
-- instance InConstantPool Text.Text where
--   deref (Index ref) = do
--     cons <- derefW ref
--     case cons of
--       String str ->
--         case TE.decodeUtf8' . unSizedByteString $ str of
--           Left (TE.DecodeError msg _) -> badEncoding ref msg (unSizedByteString str)
--           Left _ -> error "This is deprecated in the api"
--           Right txt -> return txt
--       a -> wrongType ref "String" a

-- instance InConstantPool ClassName where
--   deref (Index ref) = do
--     cons <- derefW ref
--     case cons of
--       ClassRef r ->
--         ClassName <$> deref r
--       a -> wrongType ref "ClassRef" a

-- instance InConstantPool (InClass MethodId) where
--   deref (Index ref) = do
--     cons <- derefW ref
--     case cons of
--       MethodRef cn rt ->
--         InClass <$> deref cn <*> deref rt
--       a -> wrongType ref "MethodRef" a

-- instance InConstantPool (InClass FieldId) where
--   deref (Index ref) = do
--     cons <- derefW ref
--     case cons of
--       FieldRef cn rt ->
--         InClass <$> deref cn <*> deref rt
--       a -> wrongType ref "FieldRef" a

-- instance InConstantPool MethodId where
--   deref (Index ref) = do
--     cons <- derefW ref
--     case cons of
--       NameAndType n (Index i) ->
--         MethodId <$> deref n <*> derefW i
--       a -> wrongType ref "NameAndType" a

-- instance InConstantPool FieldId where
--   deref (Index ref) = do
--     cons <- derefW ref
--     case cons of
--       NameAndType n (Index i) ->
--         FieldId <$> deref n <*> derefW i
--       a -> wrongType ref "NameAndType" a

-- instance InConstantPool MethodDescriptor where
--   deref (Index ref) = do
--     txt <- derefW ref
--     case methodDescriptorFromText txt of
--       Just x -> return x
--       Nothing -> parseError ref txt

-- instance InConstantPool FieldDescriptor where
--   deref (Index ref) = do
--     txt <- derefW ref
--     case fieldDescriptorFromText txt of
--       Just x -> return x
--       Nothing -> parseError ref txt

-- -- Helper functions

-- -- | Given a function from an object b to an 'Index a' create a function from b
-- -- to 'PoolAccess a'. Very useful for defining functions.
-- derefF :: InConstantPool a => (b -> Index a) -> b -> PoolAccess a
-- derefF fn b = deref (fn b)

-- -- Hidden access method.

-- derefW :: InConstantPool a => Word16 -> PoolAccess a
-- derefW w = deref (Index w)

-- -- Hidden exception methods

-- wrongType :: Word16 -> String -> Constant -> PoolAccess a
-- wrongType w expected got = PoolAccess $ \_ ->
--   Left (PoolAccessError w ("Expected '" ++ expected ++ "' but got '" ++ typeToStr got ++ "'"))

-- outofbounds :: Word16 -> PoolAccess a
-- outofbounds w = PoolAccess $ \_ ->
--   Left (PoolAccessError w "Out of bounds")

-- parseError :: Word16 -> Text.Text -> PoolAccess a
-- parseError w t = PoolAccess $ \_ ->
--   Left (PoolAccessError w $ "Could not parse '" ++ Text.unpack t ++ "'")

-- badEncoding :: Word16 -> String -> BS.ByteString -> PoolAccess a
-- badEncoding w str bs = PoolAccess $ \_ ->
--   Left (PoolAccessError w $ "Could not encode '" ++ str ++ "': " ++ show bs)

$(deriveShow1 ''Index)
$(deriveShow1 ''Deref)
$(deriveEq1 ''Index)
$(deriveEq1 ''Deref)
