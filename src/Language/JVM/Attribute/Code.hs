{-|
Module      : Language.JVM.Attribute.Code
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Language.JVM.Attribute.Code
  ( Code (..)
  , CodeAttributes (..)
  , ExceptionTable (..)
  , codeStackMapTable
  , codeByteCodeOprs
  , codeByteCodeInsts
  ) where

import           GHC.Generics                           (Generic)

import           Numeric                                (showHex)
import           Prelude                                hiding (fail)

import           Control.DeepSeq                        (NFData)
import           Control.Monad                          hiding (fail)
import           Control.Monad.Fail                     (fail)
import           Unsafe.Coerce

import           Data.Binary
import           Data.Binary.Get                        hiding (Get, label)
import           Data.Binary.Put                        hiding (Put)
import qualified Data.ByteString.Lazy                   as BL
import           Data.Int
import           Data.Monoid
import qualified Data.Vector                            as V
-- import           Debug.Trace

import           Language.JVM.Attribute.Base
import           Language.JVM.Attribute.LineNumberTable
import           Language.JVM.Attribute.StackMapTable
import           Language.JVM.ByteCode
import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.Utils

-- | 'Code' is an Attribute.
instance IsAttribute Code where
  attrName = Const "Code"

-- | Code contains the actual byte-code. The 'i' type parameter is added to
-- allow indicate the two stages of the code file, before and after access to
-- the 'ConstantPool'. i should be either 'Ref' or 'Deref'.
data Code r = Code
  { codeMaxStack       :: !(Word16)
  , codeMaxLocals      :: !(Word16)
  , codeByteCode       :: !((ByteCode r))
  , codeExceptionTable :: !(SizedList16 (ExceptionTable r))
  , codeAttributes     :: !(Choice r (SizedList16 (Attribute r)) (CodeAttributes r))
  }

data ExceptionTable r = ExceptionTable
  { start     :: ! (ByteCodeIndex r)
  -- ^ Inclusive program counter into 'code'
  , end       :: ! (ByteCodeIndex r)
  -- ^ Exclusive program counter into 'code'
  , handler   :: ! (ByteCodeIndex r)
  -- ^ A program counter into 'code' indicating the handler.
  , catchType :: ! (Ref (Maybe ClassName) r)
  }

-- | Extracts a list of bytecode operation
codeByteCodeOprs :: Code High -> [ByteCodeOpr High]
codeByteCodeOprs =
  unByteCode . codeByteCode

-- | Extracts a list of bytecode instructions
codeByteCodeInsts :: Code Low -> [ByteCodeInst Low]
codeByteCodeInsts =
  unByteCode . codeByteCode

-- | Returns the StackMapTable attribute if any
codeStackMapTable :: Code High -> Maybe (StackMapTable High)
codeStackMapTable =
  firstOne . caStackMapTable . codeAttributes

data CodeAttributes r = CodeAttributes
  { caStackMapTable   :: [ StackMapTable r ]
  , caLineNumberTable :: [ LineNumberTable r ]
  , caOthers          :: [ Attribute r ]
  }


instance Staged Code where
  evolve Code{..} = label "Code" $ do
    codeByteCode <- evolve codeByteCode
    codeExceptionTable <- mapM evolve codeExceptionTable
    codeAttributes <- fromCollector <$> fromAttributes collect' codeAttributes
    return $ Code {..}
    where
      fromCollector (a, b, c) =
        CodeAttributes (appEndo a []) (appEndo b []) (appEndo c [])
      collect' attr =
        collect (mempty, mempty, Endo (attr:)) attr
          [ toC $ \e -> (Endo (e:), mempty, mempty)
          , toC $ \e -> (mempty, Endo (e:), mempty)
          ]
  devolve Code{..} = do
    codeByteCode <- devolve codeByteCode
    codeExceptionTable <- mapM devolve codeExceptionTable
    codeAttributes <- SizedList <$> fromCodeAttributes codeAttributes
    return $ Code {..}
    where
      fromCodeAttributes (CodeAttributes a b c) = do
        a' <- mapM toAttribute a
        b' <- mapM toAttribute b
        c' <- mapM devolve c
        return (a' ++ b' ++ c')


instance Staged ExceptionTable where
  evolve ExceptionTable{..} = label "ExceptionTable" $ do
    catchType <- case idx catchType of
      0 -> return $ RefV Nothing
      n -> RefV . Just <$> link n
    return $ ExceptionTable {..}

  devolve ExceptionTable{..} = do
    catchType <- case value catchType of
      Just s  -> RefI <$> unlink s
      Nothing -> return $ RefI 0
    return $ ExceptionTable {..}

$(deriveBaseWithBinary ''Code)
$(deriveBaseWithBinary ''ExceptionTable)
$(deriveBase ''CodeAttributes)
