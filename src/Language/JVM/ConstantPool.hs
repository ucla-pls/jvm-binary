{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-|
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'ConstantPool' data structure and multiple
other types, and classes.
-}
module Language.JVM.ConstantPool
  (
  -- * Constant Pool
  -- $ConstantPool
    ConstantPool (..)
  , PoolAccessError (..)
  , access
  , append

  , Stage (..)
  , Stager
  , Staged (..)
  , Referenceable (..)
  
  , module Language.JVM.TH
  , module Language.JVM.Constant
  ) where

import           Control.DeepSeq          (NFData)
import           Control.Monad.Except
-- -- import           Control.Monad.Reader
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
-- -- import           Data.Monoid

import           Data.Text.Encoding       as TE
import           Data.Text.Encoding.Error as TE

import qualified Data.ByteString          as BS
import           Data.IntMap              as IM
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)

import           Language.JVM.Constant
import           Language.JVM.TH
import           Language.JVM.Type
import           Language.JVM.Utils


-- $ConstantPool
-- The 'ConstantPool' contains all the constants, and is accessible using the
-- Lookup methods.

-- | A ConstantPool is just an 'IntMap'. A 'IntMap' is used, because constants are
-- accessed using their byte-offset, and sometimes the offset depends on the constant
-- size. See 'constantSize'.
newtype ConstantPool r = ConstantPool
  { unConstantPool :: IM.IntMap (Constant r)
  }

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

-- | A pool access error
data PoolAccessError = PoolAccessError
  { paErrorRef :: !Word16
  , paErrorMsg :: String
  } deriving (Show, Eq, Generic)

instance NFData PoolAccessError

-- | Access a constant in the constant pool
access :: Word16 -> ConstantPool r -> Either PoolAccessError (Constant r)
access ref (ConstantPool cp) =
  case IM.lookup (fromIntegral ref) cp of
    Just x -> Right x
    Nothing -> Left $ PoolAccessError ref "No such element."

-- | Append a constant to the constant pool, and get the offset.
append :: Constant r -> ConstantPool r -> (Word16, ConstantPool r)
append c (ConstantPool cp) =
  (fromIntegral i, ConstantPool $ IM.insert i c cp)
  where
    i =
      case IM.toDescList cp of
        (k, a):_ ->
          k + constantSize a
        _ -> 0


-- The staging system
type Stager m s' s =
  forall a. (Monad m, Referenceable a) => Ref s' a -> m (Ref s a)

class Stage s where
  fromDeref :: Stager m Deref s
  asValue :: Ref s a -> Either (Ref Index a) a
  -- sbuild :: (a -> m Word16) -> Ref s' a -> m (Ref s a)
  -- sbuild = undefined
  -- sderef :: (Ref Index a -> m (Ref Value a)) -> Ref s' a -> m (Ref s a)
  -- sderef = undefined

  smapM :: (Monad m) => (a -> m a') -> Ref s a -> m (Ref s a')
  restage :: (Monad m, Stage s', Staged g) => Stager m s' s -> s (g s') -> m (s (g s))
  deepreref ::
    (Monad m, Stage s', Staged g, Referenceable (g s), Referenceable (g s'))
    => Stager m s' s
    -> Ref s' (g s')
    -> m (Ref s (g s))
  deepreref f r = do
    r' <- restage f . unref =<< f r
    return $ Ref r'

instance Stage Index where
  restage _ (Index w) = pure $ (Index w)
  smapM _ (Ref (Index w)) = return $ Ref (Index w)
  asValue s = Left s
  fromDeref (Ref (Deref w _)) = return $ (Ref (Index w))

instance Stage Deref where
  restage f (Deref w v) =
    Deref w <$> stage f v
  asValue = Right . getValue
  smapM f (Ref (Deref w v)) = do
    x <- f v
    return $ Ref (Deref w x)
  fromDeref (Ref (Deref w v)) =
    return $ Ref (Deref w v)

instance Stage Value where
  restage f (Value v) =
    Value <$> stage f v
  asValue = Right . getValue
  smapM f (Ref (Value v)) = do
    x <- f v
    return $ Ref (Value x)
  fromDeref (Ref (Deref _ v)) =
    return $ Ref (Value v)

class Staged f where
  stage :: (Stage s, Stage s', Monad m) => Stager m s' s -> f s' -> m (f s)

instance Staged ConstantPool where
  stage f (ConstantPool im) =
    ConstantPool <$> traverse (stage f) im

instance Staged Constant where
  stage f c =
    case c of
      CString s -> pure $ CString s
      CInteger i -> pure $ CInteger i
      CFloat d -> pure $ CFloat d
      CLong l -> pure $ CLong l
      CDouble d -> pure $ CDouble d
      CClassRef r ->
        CClassRef <$> f r
      CStringRef r ->
        CStringRef <$> f r
      CFieldRef r ->
        CFieldRef <$> stage f r
      CMethodRef r ->
        CMethodRef <$> stage f r
      CInterfaceMethodRef r ->
        CInterfaceMethodRef <$> stage f r
      CNameAndType r1 r2 ->
        CNameAndType <$> f r1 <*> f r2
      CMethodHandle mh ->
        CMethodHandle <$> stage f mh
      CMethodType r ->
        CMethodType <$> f r
      CInvokeDynamic i ->
        CInvokeDynamic <$> stage f i

instance Staged (InClass FieldId) where
  stage f (InClass cn cid) = do
    cn' <- f cn
    cid' <- deepreref f cid
    return $ InClass cn' cid'

instance Staged (InClass MethodId) where
  stage f (InClass cn cid) = do
    cn' <- f cn
    cid' <- deepreref f cid
    return $ InClass cn' cid'

instance Staged FieldId where
  stage f (FieldId fi fd) =
    FieldId <$> f fi <*> f fd

instance Staged MethodId where
  stage f (MethodId mi md) =
    MethodId <$> f mi <*> f md

instance Staged MethodHandle where
  stage f m =
    case m of
      MHField r ->
        MHField <$> stage f r
      MHMethod r ->
        MHMethod <$> stage f r
      MHInterface r ->
        MHInterface <$> stage f r

instance Staged InvokeDynamic where
  stage f (InvokeDynamic w m) =
    InvokeDynamic w <$> deepreref f m

instance Staged MethodHandleMethod where
  stage f (MethodHandleMethod k ref) =
    MethodHandleMethod k <$> deepreref f ref

instance Staged MethodHandleField where
  stage f (MethodHandleField k ref) =
    MethodHandleField k <$> deepreref f ref

instance Staged MethodHandleInterface where
  stage f (MethodHandleInterface ref) =
    MethodHandleInterface <$> deepreref f ref

wrongType :: Show1 r => String -> Constant r -> String
wrongType n c =
  "Expected '" ++ n ++ "', but found'" ++ typeToStr c ++ "'."

badEncoding :: String -> BS.ByteString -> String
badEncoding str bs =
  "Could not encode '" ++ str ++ "': " ++ show bs


-- | 'Referenceable' is something that can exist in the constant pool.
class Referenceable a where
  fromConst
    :: (Monad m)
    => --(forall s. Stage s => Stager m Deref s) ->
       (forall a'. String -> m a')
    -> Constant Deref
    -> m a
  toConst
    :: (Monad m)
    => (forall s. Stage s => Stager m s Index)
    -> a
    -> m (Constant Index)
  toConst _ = undefined

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

  toConst _ txt =
    return $ CString (SizedByteString $ TE.encodeUtf8 txt)

instance Referenceable ClassName where
  fromConst _ (CClassRef r) =
    return . ClassName $ getValue r
  fromConst err a =
    err $ wrongType "ClassRef" a

  toConst f (ClassName txt) = do
    r <- f $ Ref . Value $ txt
    return $ CClassRef r


instance Stage s => Referenceable (Constant s) where
  fromConst _ c =
    stage fromDeref c
  toConst f c = do
    stage f c

instance Referenceable FieldDescriptor where
  fromConst err c = do
    txt <- fromConst err c
    either err return $ fieldDescriptorFromText txt

  -- toConst f c = state f c


instance Stage s => Referenceable (FieldId s) where
  fromConst err (CNameAndType rn txt) = do
    rn' <- fromDeref rn
    t' <- fromDeref txt
    txt' <- smapM (either err return . fieldDescriptorFromText) t'
    return $ FieldId rn' txt'
  fromConst err c = err $ wrongType "NameAndType" c

instance Referenceable MethodDescriptor where
  fromConst err c = do
    txt <- fromConst err c
    either err return $ methodDescriptorFromText txt

instance Stage s => Referenceable (MethodId s) where
  fromConst err (CNameAndType rn txt) = do
    rn' <- fromDeref rn
    t' <- fromDeref txt
    txt' <- smapM (either err return . methodDescriptorFromText) t'
    return $ MethodId rn' txt'
  fromConst err c = err $ wrongType "NameAndType" c

instance Stage s => Referenceable (InClass FieldId s) where
  fromConst _ (CFieldRef s) = do
    stage fromDeref s
  fromConst err c =
    err $ wrongType "FieldRef" c

instance Stage s => Referenceable (InClass MethodId s) where
  fromConst _ (CMethodRef s) = do
    stage fromDeref s
  fromConst err c =
    err $ wrongType "MethodRef" c

$(deriveBase ''ConstantPool)

