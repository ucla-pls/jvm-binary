{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-|
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the stages, there are two stages; 'Low' and 'High'. 'Low'
represents closest to the metal and 'High' represents closer to the conceptual
representation.
-}
module Language.JVM.Stage
  ( Staged (..)

  -- * Monad Classes
  , EvolveM (..)
  , DevolveM (..)
  -- * Re-exports
  , DeepRef
  , Ref
  , module Language.JVM.TH
  ) where

import Language.JVM.Constant
import Language.JVM.TH

class Monad m => EvolveM m where
  link :: Referenceable r => Index -> m r
  attributeError :: String -> m r

class Monad m => DevolveM m where
  unlink :: Referenceable r => r -> m Index

class Staged s where
  {-# MINIMAL stage | evolve, devolve #-}
  stage :: Monad m => (forall s'. Staged s' => s' r -> m (s' r')) -> s r -> m (s r')
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
      CClassRef r -> CClassRef <$> f r
      CStringRef r -> CStringRef <$> f r
      CFieldRef r -> CFieldRef <$> f r
      CMethodRef r -> CMethodRef <$> f r
      CInterfaceMethodRef r -> CInterfaceMethodRef <$> f r
      CNameAndType r1 r2 -> CNameAndType <$> f r1 <*> f r2
      CMethodHandle mh -> CMethodHandle <$> f mh
      CMethodType r -> CMethodType <$> f r
      CInvokeDynamic i -> CInvokeDynamic <$> f i

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
  stage f (MethodHandleMethod k ref) =
    MethodHandleMethod k <$> f ref

instance Staged MethodHandleField where
  stage f (MethodHandleField k ref) =
    MethodHandleField k <$> f ref

instance Staged MethodHandleInterface where
  stage f (MethodHandleInterface ref) =
    MethodHandleInterface <$> f ref
