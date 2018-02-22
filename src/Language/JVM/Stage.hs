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
  evolve ::  EvolveM m => s Low -> m (s High)
  devolve :: DevolveM m => s High -> m (s Low)

instance Staged Constant where
  evolve c =
    case c of
      CString s -> pure $ CString s
      CInteger i -> pure $ CInteger i
      CFloat d -> pure $ CFloat d
      CLong l -> pure $ CLong l
      CDouble d -> pure $ CDouble d
      CClassRef r -> CClassRef <$> evolve r
      CStringRef r -> CStringRef <$> evolve r
      CFieldRef r -> CFieldRef <$> evolve r
      CMethodRef r -> CMethodRef <$> evolve r
      CInterfaceMethodRef r -> CInterfaceMethodRef <$> evolve r
      CNameAndType r1 r2 -> CNameAndType <$> evolve r1 <*> evolve r2
      CMethodHandle mh -> CMethodHandle <$> evolve mh
      CMethodType r -> CMethodType <$> evolve r
      CInvokeDynamic i -> CInvokeDynamic <$> evolve i
  devolve c =
    case c of
      CString s -> pure $ CString s
      CInteger i -> pure $ CInteger i
      CFloat d -> pure $ CFloat d
      CLong l -> pure $ CLong l
      CDouble d -> pure $ CDouble d
      CClassRef r -> CClassRef <$> devolve r
      CStringRef r -> CStringRef <$> devolve r
      CFieldRef r -> CFieldRef <$> devolve r
      CMethodRef r -> CMethodRef <$> devolve r
      CInterfaceMethodRef r -> CInterfaceMethodRef <$> devolve r
      CNameAndType r1 r2 -> CNameAndType <$> devolve r1 <*> devolve r2
      CMethodHandle mh -> CMethodHandle <$> devolve mh
      CMethodType r -> CMethodType <$> devolve r
      CInvokeDynamic i -> CInvokeDynamic <$> devolve i

instance Referenceable r => Staged (Ref r) where
  evolve (RefI ref) = do
    RefV <$> link ref
  devolve (RefV v) = do
    RefI <$> unlink v

instance Referenceable (r High) => Staged (DeepRef r) where
  evolve (DeepRef (RefI ref)) = do
    DeepRef . RefV <$> link ref
  devolve (DeepRef (RefV v)) = do
    DeepRef . RefI <$> unlink v

instance Staged InvokeDynamic where
  evolve (InvokeDynamic w ref) =
    InvokeDynamic w <$> evolve ref
  devolve (InvokeDynamic w ref) =
    InvokeDynamic w <$> devolve ref

instance Staged MethodId where
  evolve (MethodId n d) =
    MethodId <$> evolve n <*> evolve d
  devolve (MethodId n d) =
    MethodId <$> devolve n <*> devolve d

instance Referenceable (r High) => Staged (InClass r) where
  evolve (InClass cn cid) = do
    InClass <$> evolve cn <*> evolve cid
  devolve (InClass cn cid) = do
    InClass <$> devolve cn <*> devolve cid

instance Staged MethodHandle where
  evolve m =
    case m of
      MHField r -> MHField <$> evolve r
      MHMethod r -> MHMethod <$> evolve r
      MHInterface r -> MHInterface <$> evolve r
  devolve m =
    case m of
      MHField r -> MHField <$> devolve r
      MHMethod r -> MHMethod <$> devolve r
      MHInterface r -> MHInterface <$> devolve r

instance Staged MethodHandleMethod where
  evolve (MethodHandleMethod k ref) =
    MethodHandleMethod k <$> evolve ref
  devolve (MethodHandleMethod k ref) =
    MethodHandleMethod k <$> devolve ref

instance Staged MethodHandleField where
  evolve (MethodHandleField k ref) =
    MethodHandleField k <$> evolve ref
  devolve (MethodHandleField k ref) =
    MethodHandleField k <$> devolve ref

instance Staged MethodHandleInterface where
  evolve (MethodHandleInterface ref) =
    MethodHandleInterface <$> evolve ref
  devolve (MethodHandleInterface ref) =
    MethodHandleInterface <$> devolve ref
