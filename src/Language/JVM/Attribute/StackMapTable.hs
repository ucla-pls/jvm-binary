{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-|
Module      : Language.JVM.Attribute.StackMapTable
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the StackMapTable Attribute,
as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.4).

-}

module Language.JVM.Attribute.StackMapTable
  ( StackMapTable (..)
  , DeltaOffset
  , StackMapFrame (..)
  , StackMapFrameType (..)
  , emptyStackMapTable

  , VerificationTypeInfo (..)

  -- * Helper functions
  , offsetDelta
  , offsetDeltaInv
  ) where

import           Control.Monad               (replicateM)
import           Data.Binary
import           Data.Binary.Get             hiding (label)
import           Data.Binary.Put
import           Data.Foldable
import           Numeric
import           Unsafe.Coerce

import           Language.JVM.Attribute.Base
import           Language.JVM.ByteCode
import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.Utils

-- | 'StackMapTable' is an Attribute.
instance IsAttribute (StackMapTable Low) where
  attrName = Const "StackMapTable"

-- | An Exceptions attribute is a list of references into the
-- constant pool.
newtype StackMapTable r = StackMapTable
  { stackMapTable :: Choice (SizedList16 (StackMapFrame Low)) [StackMapFrame High] r
  }

emptyStackMapTable :: StackMapTable High
emptyStackMapTable = StackMapTable []

-- | A delta offset
type DeltaOffset i = Choice Word16 Int i

-- | An stack map frame
data StackMapFrame r = StackMapFrame
  { deltaOffset :: DeltaOffset r
  , frameType   :: StackMapFrameType r
  }

-- | An stack map frame type
data StackMapFrameType r
  = SameFrame
  | SameLocals1StackItemFrame (VerificationTypeInfo r)
  | ChopFrame Word8
  | AppendFrame [VerificationTypeInfo r]
  | FullFrame
      (SizedList16 (VerificationTypeInfo r))
      (SizedList16 (VerificationTypeInfo r))

instance Binary (StackMapFrame Low) where
  get = do
    ft <- getWord8
    let
      framegetter
        | 0 <= ft && ft <= 63
        = return $ StackMapFrame (fromIntegral ft) SameFrame

        | 64 <= ft && ft <= 127
        = StackMapFrame (fromIntegral $ ft - 64) . SameLocals1StackItemFrame <$> get

        | 128 <= ft && ft <= 246
        = fail $ "Reserved for further use: '0x" ++ showHex ft "'"

        | ft == 247
        = StackMapFrame <$> getWord16be <*> (SameLocals1StackItemFrame <$> get)

        | 248 <= ft && ft <= 250
        = StackMapFrame <$> getWord16be <*> pure (ChopFrame (251 - ft))

        | ft == 251
        = StackMapFrame <$> getWord16be <*> pure SameFrame

        | 252 <= ft && ft <= 254
        = do
            offset' <- getWord16be
            locals <- replicateM (fromIntegral $ ft - 251) get
            return $ StackMapFrame offset' (AppendFrame locals)

        | ft == 255
        = StackMapFrame <$> getWord16be <*> (FullFrame <$> get <*> get)

        | otherwise
        = fail $ "Unknown frame type '0x" ++ showHex ft "'"
    framegetter

  put (StackMapFrame off frame) = do
    case frame of

      SameFrame
        | off <= 63 ->
            putWord8 (fromIntegral off)
        | otherwise -> do
            putWord8 251
            putWord16be off

      SameLocals1StackItemFrame vt
        | off <= 63 -> do
            putWord8 $ fromIntegral (64 + off)
            put vt
        | otherwise -> do
            putWord8 247
            putWord16be off
            put vt

      ChopFrame w
        | 0 < w && w <= 3 -> do
          putWord8 (251 - w)
          putWord16be off
        | otherwise ->
          fail $ "Can't write a cutoff value outside ]0,3], but was: " ++ show w

      AppendFrame vs
        | length vs <= 3 && 0 < length vs -> do
          putWord8 (fromIntegral $ 251 + length vs)
          putWord16be off
          mapM_ put vs
        | otherwise ->
          fail $ "The AppendFrame has to contain at least 1 and at most 3 elements: " ++ show vs

      FullFrame ls1 ls2 -> do
        putWord8 255
        putWord16be off
        put ls1
        put ls2

-- TODO: Fix that ClassName can be an array.
-- | The types info of the stack map frame.
data VerificationTypeInfo r
  = VTTop
  | VTInteger
  | VTFloat
  | VTLong
  | VTDouble
  | VTNull
  | VTUninitializedThis
  | VTObject (Ref ClassName r)
  | VTUninitialized !Word16


instance Binary (VerificationTypeInfo Low) where
  get = do
    tag <- getWord8
    case tag of
      0 -> pure VTTop
      1 -> pure VTInteger
      2 -> pure VTFloat
      3 -> pure VTLong
      4 -> pure VTDouble
      5 -> pure VTNull
      6 -> pure VTUninitializedThis
      7 -> VTObject <$> get
      8 -> VTUninitialized <$> get
      _ -> fail $ "Unexpected tag : '0x" ++ showHex tag "'"

  put a = do
    case a of
      VTTop               -> putWord8 0
      VTInteger           -> putWord8 1
      VTFloat             -> putWord8 2
      VTLong              -> putWord8 3
      VTDouble            -> putWord8 4
      VTNull              -> putWord8 5
      VTUninitializedThis -> putWord8 6
      VTObject s          -> do putWord8 7; put s
      VTUninitialized s   -> do putWord8 8; put s

instance ByteCodeStaged StackMapTable where
  evolveBC f (StackMapTable ls) =
    label "StackMapTable" $
    StackMapTable . reverse . snd <$> foldl' acc (return (0, [])) ls
    where
      acc a (StackMapFrame delta frm) = do
        (lidx, lst) <- a
        frm' <- evolve frm
        let bco = if lst /= [] then offsetDelta lidx delta else delta
        x <- f bco
        return (bco, StackMapFrame x frm' : lst)

  devolveBC f (StackMapTable ls) =
    label "StackMapTable" $
    StackMapTable . SizedList . reverse . snd <$> foldl' acc (return (0 :: Word16, [])) ls
    where
      acc a (StackMapFrame x frm) = do
        (lidx, lst) <- a
        frm' <- devolve frm
        tidx <- f x
        let delta = if lst /= [] then offsetDeltaInv lidx tidx else tidx
        return (tidx, StackMapFrame delta frm' : lst)


offsetDelta ::
  Word16
  -- ^ Last Index
  -> Word16
  -- ^ Delta
  -> Word16
  -- ^ This Index
offsetDelta lidx delta
  = lidx + delta + 1

offsetDeltaInv ::
  Word16
  -- ^ Last Index
  -> Word16
  -- ^ Current Index
  -> Word16
  -- ^ Delta
offsetDeltaInv lidx tidx
  = tidx - lidx - 1

instance Staged StackMapFrameType where
  stage f x =
    case x of
      SameLocals1StackItemFrame a -> SameLocals1StackItemFrame <$> f a
      AppendFrame ls              -> AppendFrame <$> mapM f ls
      FullFrame bs as             -> FullFrame <$> mapM f bs <*> mapM f as
      a                           -> return $ unsafeCoerce a

instance Staged VerificationTypeInfo where
  devolve x =
    case x of
      VTObject a -> VTObject <$> unlink a
      a         -> return $ unsafeCoerce a

  evolve x =
    case x of
      VTObject a -> VTObject <$> link a
      a         -> return $ unsafeCoerce a


$(deriveBaseWithBinary ''StackMapTable)
$(deriveBase ''StackMapFrame)
$(deriveBase ''StackMapFrameType)
$(deriveBase ''VerificationTypeInfo)
