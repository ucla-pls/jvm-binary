{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-|
Module      : Language.JVM.Attribute.StackMapTable
Copyright   : (c) Christian Gram Kalhauge, 2017
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
  ) where

import           Data.Binary
import           Numeric
import           Control.Monad (replicateM)

import           Language.JVM.Constant
import           Language.JVM.Stage
import           Language.JVM.Utils

-- | An Exceptions attribute is a list of references into the
-- constant pool.
data StackMapTable r = StackMapTable
  { stackMapTable :: SizedList16 (StackMapFrame r)
  }

-- | A delta offset
type DeltaOffset = Word8

-- | An stack map frame
data StackMapFrame r = StackMapFrame
  { deltaOffset :: DeltaOffset
  , frameType :: StackMapFrameType r
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
        = return $ StackMapFrame ft SameFrame

        | 64 <= ft && ft <= 127
        = StackMapFrame (ft - 64) . SameLocals1StackItemFrame <$> get

        | 128 <= ft && ft <= 246
        = fail $ "Reserved for further use: '0x" ++ showHex ft "'"

        | ft == 247
        = StackMapFrame <$> get <*> (SameLocals1StackItemFrame <$> get)

        | 248 <= ft && ft <= 250
        = StackMapFrame <$> get <*> pure (ChopFrame (251 - ft))

        | ft == 251
        = StackMapFrame <$> get <*> pure SameFrame

        | 252 <= ft && ft <= 254
        = do
            offset <- get
            locals <- replicateM (fromIntegral $ ft - 251) get
            return $ StackMapFrame offset (AppendFrame locals)

        | ft == 255
        = StackMapFrame <$> get <*> (FullFrame <$> get <*> get)

        | otherwise
        = fail $ "Unknown frame type '0x" ++ showHex ft "'"
    framegetter

  put (StackMapFrame off frame) = do
    case frame of

      SameFrame
        | off <= 63 ->
            putWord8 off
        | otherwise -> do
            putWord8 251
            putWord8 off

      SameLocals1StackItemFrame vt
        | off <= 63 -> do
            putWord8 (64 + off)
            put vt
        | otherwise -> do
            putWord8 247
            putWord8 off
            put vt

      ChopFrame w
        | 0 < w && w <= 3 -> do
          putWord8 (251 - w)
          putWord8 off
        | otherwise ->
          fail $ "Can't write a cutoff value outside ]0,3], but was: " ++ show w

      AppendFrame vs
        | length vs <= 3 && 0 < length vs -> do
          putWord8 (fromIntegral $ 251 + length vs)
          putWord8 off
          mapM_ put vs
        | otherwise ->
          fail $ "The AppendFrame has to contain at least 1 and at most 3 elements: " ++ show vs

      FullFrame ls1 ls2 -> do
        putWord8 255
        put off
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
      VTop -> putWord8 0
      VInteger -> putWord8 1
      VFloat -> putWord8 2
      VLong -> putWord8 3
      VDouble -> putWord8 4
      VNull -> putWord8 5
      VUninitializedThis -> putWord8 6
      VObject s -> do putWord8 7; put s
      VUninitialized s -> do putWord8 8; put s

$(deriveBaseWithBinary ''StackMapTable)
$(deriveBase ''StackMapFrame)
$(deriveBase ''StackMapFrameType)
$(deriveBase ''VerificationTypeInfo)
