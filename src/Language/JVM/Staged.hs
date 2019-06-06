{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

  -- * AttributeLocation
  , AttributeLocation (..)

  -- * Re-exports
  , module Language.JVM.Stage
  , module Language.JVM.TH
  ) where

import qualified Data.Text as Text

import Language.JVM.Constant
import Language.JVM.Stage
import Language.JVM.TH

class Monad m => LabelM m where
  label :: String -> m a -> m a
  -- ^ label the current position in the class-file, good for debugging
  label _ = id
  {-# INLINE label #-}

data AttributeLocation
  = ClassAttribute
  | MethodAttribute
  | CodeAttribute
  | FieldAttribute
  deriving (Show, Eq, Ord)

class LabelM m => EvolveM m where
  link :: Referenceable r => Index -> m r
  attributeFilter :: m ((AttributeLocation, Text.Text) -> Bool)
  attributeError :: String -> m r

class LabelM m => DevolveM m where
  unlink :: Referenceable r => r -> m Index

class Staged s where
  {-# MINIMAL stage | evolve, devolve #-}
  stage :: LabelM m => (forall s'. Staged s' => s' r -> m (s' r')) -> s r -> m (s r')
  stage f a = f a
  {-# INLINE stage #-}

  evolve ::  EvolveM m => s Low -> m (s High)
  evolve = stage evolve
  {-# INLINE evolve #-}

  devolve :: DevolveM m => s High -> m (s Low)
  devolve = stage devolve
  {-# INLINE devolve #-}

instance Staged Constant where
  evolve c =
    case c of
      CString s -> pure $ CString s
      CInteger i -> pure $ CInteger i
      CFloat d -> pure $ CFloat d
      CLong l -> pure $ CLong l
      CDouble d -> pure $ CDouble d
      CClassRef r -> label "CClassRef" $ CClassRef <$> link r
      CStringRef r -> label "CStringRef" $ CStringRef <$> link r
      CFieldRef r -> label "CFieldRef" $ CFieldRef <$> evolve r
      CMethodRef r -> label "CMethodRef" $ CMethodRef <$> evolve r
      CInterfaceMethodRef r -> label "CInterfaceMethodRef" $ CInterfaceMethodRef <$> evolve r
      CNameAndType r1 r2 -> label "CNameAndType" $ CNameAndType <$> link r1 <*> link r2
      CMethodHandle mh -> label "CMethodHandle" $ CMethodHandle <$> evolve mh
      CMethodType r -> label "CMethodType" $ CMethodType <$> link r
      CInvokeDynamic i -> label "CInvokeDynamic" $ CInvokeDynamic <$> evolve i

  devolve c =
    case c of
      CString s -> pure $ CString s
      CInteger i -> pure $ CInteger i
      CFloat d -> pure $ CFloat d
      CLong l -> pure $ CLong l
      CDouble d -> pure $ CDouble d
      CClassRef r -> label "CClassRef" $ CClassRef <$> unlink r
      CStringRef r -> label "CStringRef" $ CStringRef <$> unlink r
      CFieldRef r -> label "CFieldRef" $ CFieldRef <$> devolve r
      CMethodRef r -> label "CMethodRef" $ CMethodRef <$> devolve r
      CInterfaceMethodRef r -> label "CInterfaceMethodRef" $ CInterfaceMethodRef <$> devolve r
      CNameAndType r1 r2 -> label "CNameAndType" $ CNameAndType <$> unlink r1 <*> unlink r2
      CMethodHandle mh -> label "CMetho" $ CMethodHandle <$> devolve mh
      CMethodType r -> label "CMethodType" $ CMethodType <$> unlink r
      CInvokeDynamic i -> label "CInvokeDynamic" $ CInvokeDynamic <$> devolve i


instance Staged InvokeDynamic where
  evolve (InvokeDynamic w ref) =
    InvokeDynamic w <$> link ref

  devolve (InvokeDynamic w ref) =
    InvokeDynamic w <$> unlink ref

-- instance Staged MethodId where
--   evolve (MethodId n d) =
--     MethodId <$> link n <*> link d

--   devolve (MethodId n d) =
--     MethodId <$> unlink n <*> unlink d

instance Referenceable r => Staged (InClass r) where
  evolve (InClass cn cid) =
    InClass <$> link cn <*> link cid
  devolve (InClass cn cid) =
    InClass <$> unlink cn <*> unlink cid

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
  evolve g =
    case g of
      MHInvokeVirtual m -> MHInvokeVirtual <$> link m
      MHInvokeStatic m -> MHInvokeStatic <$> link m
      MHInvokeSpecial m -> MHInvokeSpecial <$> link m
      MHNewInvokeSpecial m -> MHNewInvokeSpecial <$> link m

  devolve g =
    case g of
      MHInvokeVirtual m -> MHInvokeVirtual <$> unlink m
      MHInvokeStatic m -> MHInvokeStatic <$> unlink m
      MHInvokeSpecial m -> MHInvokeSpecial <$> unlink m
      MHNewInvokeSpecial m -> MHNewInvokeSpecial <$> unlink m

instance Staged MethodHandleField where
  evolve (MethodHandleField k ref) =
    MethodHandleField k <$> link ref

  devolve (MethodHandleField k ref) =
    MethodHandleField k <$> unlink ref

instance Staged MethodHandleInterface where
  evolve (MethodHandleInterface ref) =
    MethodHandleInterface <$> link ref

  devolve (MethodHandleInterface ref) =
    MethodHandleInterface <$> unlink ref
