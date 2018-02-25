{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE OverloadedStrings   #-}
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
import           Data.Binary.Get
import           Data.Binary.Put
import           Numeric
import           Control.Monad (replicateM)

import           Language.JVM.Constant
import           Language.JVM.Stage
import           Language.JVM.Utils
import           Language.JVM.Attribute.Base

-- | 'StackMapTable' is an Attribute.
instance IsAttribute StackMapTable where
  attrName = Const "StackMapTable"

-- | An Exceptions attribute is a list of references into the
-- constant pool.
newtype StackMapTable r = StackMapTable
  { stackMapTable :: SizedList16 (StackMapFrame r)
  }

-- | A delta offset
type DeltaOffset = Word16

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
            offset <- getWord16be
            locals <- replicateM (fromIntegral $ ft - 251) get
            return $ StackMapFrame offset (AppendFrame locals)

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
      VTop -> putWord8 0
      VInteger -> putWord8 1
      VFloat -> putWord8 2
      VLong -> putWord8 3
      VDouble -> putWord8 4
      VNull -> putWord8 5
      VUninitializedThis -> putWord8 6
      VObject s -> do putWord8 7; put s
      VUninitialized s -> do putWord8 8; put s

instance Staged StackMapTable where
  stage f (StackMapTable ls) =
    StackMapTable <$> mapM f ls

instance Staged StackMapFrame where
  stage f (StackMapFrame so ft) =
    StackMapFrame so <$> f ft

instance Staged StackMapFrameType where
  stage f x =
    case x of
      SameFrame -> return $ SameFrame
      SameLocals1StackItemFrame a -> SameLocals1StackItemFrame <$> f a
      ChopFrame w -> return $ ChopFrame w
      AppendFrame ls -> AppendFrame <$> mapM f ls
      FullFrame bs as -> FullFrame <$> mapM f bs <*> mapM f as

instance Staged VerificationTypeInfo where
  stage f x =
    case x of
      VTop -> return $ VTop
      VInteger -> return $ VInteger
      VFloat -> return $ VFloat
      VLong -> return $ VLong
      VDouble -> return $ VDouble
      VNull -> return $ VNull
      VUninitializedThis -> return $ VUninitializedThis
      VObject a -> VObject <$> f a
      VUninitialized w -> return $ VUninitialized w


$(deriveBaseWithBinary ''StackMapTable)
$(deriveBase ''StackMapFrame)
$(deriveBase ''StackMapFrameType)
$(deriveBase ''VerificationTypeInfo)
