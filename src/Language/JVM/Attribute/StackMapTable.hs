{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
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
  { stackMapTable :: Choice r (SizedList16 (StackMapFrame Low)) [StackMapFrame High]
  }

-- | A delta offset
type DeltaOffset i = Choice i Word16 Int

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

-- | The types info of the stack map frame.
data VerificationTypeInfo r
  = VTop
  | VInteger
  | VFloat
  | VLong
  | VDouble
  | VNull
  | VUninitializedThis
  | VObject (Ref ClassName r)
  | VUninitialized !Word16


instance Binary (VerificationTypeInfo Low) where
  get = do
    tag <- getWord8
    case tag of
      0 -> pure VTop
      1 -> pure VInteger
      2 -> pure VFloat
      3 -> pure VLong
      4 -> pure VDouble
      5 -> pure VNull
      6 -> pure VUninitializedThis
      7 -> VObject <$> get
      8 -> VUninitialized <$> get
      _ -> fail $ "Unexpected tag : '0x" ++ showHex tag "'"

  put a = do
    case a of
      VTop               -> putWord8 0
      VInteger           -> putWord8 1
      VFloat             -> putWord8 2
      VLong              -> putWord8 3
      VDouble            -> putWord8 4
      VNull              -> putWord8 5
      VUninitializedThis -> putWord8 6
      VObject s          -> do putWord8 7; put s
      VUninitialized s   -> do putWord8 8; put s

instance ByteCodeStaged StackMapTable where
  evolveBC f (StackMapTable ls) =
    label "StackMapTable" $
    StackMapTable . reverse . snd <$> foldl' acc (return (0, [])) ls
    where
      acc a (StackMapFrame delta frm) = do
        (lidx, lst) <- a
        frm' <- evolve frm
        let bco = offsetDelta lidx delta
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
        let delta = offsetDeltaInv lidx tidx
        return (tidx, StackMapFrame delta frm' : lst)


offsetDelta ::
  Word16
  -- ^ Last Index
  -> Word16
  -- ^ Delta
  -> Word16
  -- ^ This Index
offsetDelta lidx delta
  | lidx > 0 = lidx + delta + 1
  | otherwise = delta

offsetDeltaInv ::
  Word16
  -- ^ Last Index
  -> Word16
  -- ^ Current Index
  -> Word16
  -- ^ Delta
offsetDeltaInv lidx tidx
  | lidx > 0 = tidx - lidx - 1
  | otherwise = tidx


instance Staged StackMapFrameType where
  stage f x =
    case x of
      SameFrame                   -> return $ SameFrame
      SameLocals1StackItemFrame a -> SameLocals1StackItemFrame <$> f a
      ChopFrame w                 -> return $ ChopFrame w
      AppendFrame ls              -> AppendFrame <$> mapM f ls
      FullFrame bs as             -> FullFrame <$> mapM f bs <*> mapM f as

instance Staged VerificationTypeInfo where
  stage f x =
    case x of
      VTop               -> return VTop
      VInteger           -> return VInteger
      VFloat             -> return VFloat
      VLong              -> return VLong
      VDouble            -> return VDouble
      VNull              -> return VNull
      VUninitializedThis -> return VUninitializedThis
      VObject a          -> VObject <$> f a
      VUninitialized w   -> return $ VUninitialized w


$(deriveBaseWithBinary ''StackMapTable)
$(deriveBase ''StackMapFrame)
$(deriveBase ''StackMapFrameType)
$(deriveBase ''VerificationTypeInfo)
