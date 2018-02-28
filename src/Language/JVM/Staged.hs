{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-|
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

-}
module Language.JVM.Staged
  ( Staged (..)

  -- * Monad Classes
  , LabelM (..)
  , EvolveM (..)
  , DevolveM (..)
  -- * Re-exports
  , module Language.JVM.Stage
  , module Language.JVM.TH
  ) where

import Language.JVM.Constant
import Language.JVM.Stage
import Language.JVM.TH

class Monad m => LabelM m where
  label :: String -> m a -> m a
  -- ^ label the current position in the class-file, good for debugging
  label _ = id

class LabelM m => EvolveM m where
  link :: Referenceable r => Index -> m r
  attributeError :: String -> m r

class LabelM m => DevolveM m where
  unlink :: Referenceable r => r -> m Index

class Staged s where
  {-# MINIMAL stage | evolve, devolve #-}
  stage :: LabelM m => (forall s'. Staged s' => s' r -> m (s' r')) -> s r -> m (s r')
  stage f a = f a

  evolve ::  EvolveM m => s Low -> m (s High)
  evolve = stage evolve

  devolve :: DevolveM m => s High -> m (s Low)
  devolve = stage devolve

instance Staged Constant where
  stage f c =
    case c of
      CString s -> pure $ CString s
      CInteger i -> pure $ CInteger i
      CFloat d -> pure $ CFloat d
      CLong l -> pure $ CLong l
      CDouble d -> pure $ CDouble d
      CClassRef r -> label "CClassRef" $ CClassRef <$> f r
      CStringRef r -> label "CStringRef" $ CStringRef <$> f r
      CFieldRef r -> label "CFieldRef" $ CFieldRef <$> f r
      CMethodRef r -> label "CMethodRef" $ CMethodRef <$> f r
      CInterfaceMethodRef r -> label "CInterfaceMethodRef" $ CInterfaceMethodRef <$> f r
      CNameAndType r1 r2 -> label "CNameAndType" $ CNameAndType <$> f r1 <*> f r2
      CMethodHandle mh -> label "CMetho" $ CMethodHandle <$> f mh
      CMethodType r -> label "CMethodType" $ CMethodType <$> f r
      CInvokeDynamic i -> label "CInvokeDynamic" $ CInvokeDynamic <$> f i

instance Referenceable r => Staged (Ref r) where
  stage _ _ = error "Cannot stage a reference"

  evolve (RefI ref) = do
    RefV <$> link ref
  devolve (RefV v) = do
    RefI <$> unlink v

instance Referenceable (r High) => Staged (DeepRef r) where
  stage _ _ = error "Cannot stage a deep reference"

  evolve (DeepRef (RefI ref)) = do
    DeepRef . RefV <$> link ref
  devolve (DeepRef (RefV v)) = do
    DeepRef . RefI <$> unlink v

instance Staged InvokeDynamic where
  stage f (InvokeDynamic w ref) =
    InvokeDynamic w <$> f ref

instance Staged MethodId where
  stage f (MethodId n d) =
    MethodId <$> f n <*> f d

instance Referenceable (r High) => Staged (InClass r) where
  stage f (InClass cn cid) = do
    InClass <$> f cn <*> f cid

instance Staged MethodHandle where
  stage f m =
    case m of
      MHField r -> MHField <$> f r
      MHMethod r -> MHMethod <$> f r
      MHInterface r -> MHInterface <$> f r

instance Staged MethodHandleMethod where
  stage f g =
    case g of
      MHInvokeVirtual m -> MHInvokeVirtual <$> f m
      MHInvokeStatic m -> MHInvokeStatic <$> f m
      MHInvokeSpecial m -> MHInvokeSpecial <$> f m
      MHNewInvokeSpecial m -> MHNewInvokeSpecial <$> f m

instance Staged MethodHandleField where
  stage f (MethodHandleField k ref) =
    MethodHandleField k <$> f ref

instance Staged MethodHandleInterface where
  stage f (MethodHandleInterface ref) =
    MethodHandleInterface <$> f ref
