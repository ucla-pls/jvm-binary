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

import           Prelude hiding (fail)

import           Data.Binary
import           Data.Monoid
import qualified Data.Vector as V

import           Language.JVM.Attribute.Base
import           Language.JVM.Attribute.LineNumberTable
import           Language.JVM.Attribute.StackMapTable
import           Language.JVM.ByteCode
import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.Utils

-- | 'Code' is an Attribute.
instance IsAttribute (Code Low) where
  attrName = Const "Code"

-- | Code contains the actual byte-code. The 'i' type parameter is added to
-- allow indicate the two stages of the code file, before and after access to
-- the 'ConstantPool'. i should be either 'Ref' or 'Deref'.
data Code r = Code
  { codeMaxStack       :: !(Word16)
  , codeMaxLocals      :: !(Word16)
  , codeByteCode       :: !((ByteCode r))
  , codeExceptionTable :: !(SizedList16 (ExceptionTable r))
  , codeAttributes     :: !(Attributes CodeAttributes r)
  }

data ExceptionTable r = ExceptionTable
  { start     :: ! (ByteCodeRef r)
  -- ^ Inclusive program counter into 'code'
  , end       :: ! (ByteCodeRef r)
  -- ^ Exclusive program counter into 'code'
  , handler   :: ! (ByteCodeRef r)
  -- ^ A program counter into 'code' indicating the handler.
  , catchType :: ! (Ref (Maybe ClassName) r)
  }

-- | Extracts a list of bytecode operation
codeByteCodeOprs :: Code High -> V.Vector (ByteCodeOpr High)
codeByteCodeOprs =
  unByteCode . codeByteCode

-- | Extracts a list of bytecode instructions
codeByteCodeInsts :: Code Low -> V.Vector (ByteCodeInst Low)
codeByteCodeInsts =
  snd . unByteCode . codeByteCode

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
    (offsets, codeByteCode) <- evolveByteCode codeByteCode
    let evolver = (evolveOffset offsets)
    codeExceptionTable <- mapM (evolveBC evolver) codeExceptionTable
    codeAttributes <- fromCollector <$> fromAttributes (collect' evolver) codeAttributes
    return $ Code {..}
    where
      fromCollector (a, b, c) =
        CodeAttributes (appEndo a []) (appEndo b []) (appEndo c [])
      collect' evolver attr =
        collect (mempty, mempty, Endo (attr:)) attr
          [ toC' (evolveBC evolver) $ \e -> (Endo (e:), mempty, mempty)
          , toC' (evolveBC evolver) $ \e -> (mempty, Endo (e:), mempty)
          ]

  devolve Code{..} = do
    codeByteCode <- devolveByteCode codeByteCode
    let bcdevolver = devolveOffset codeByteCode
    codeExceptionTable <-
      mapM (devolveBC bcdevolver) codeExceptionTable
    codeAttributes <- SizedList <$> fromCodeAttributes bcdevolver codeAttributes
    return $ Code {..}
    where
      fromCodeAttributes bcdevolver (CodeAttributes a b c) = do
        a' <- mapM (devolveAttribute (devolveBC bcdevolver)) a
        b' <- mapM (devolveAttribute (devolveBC bcdevolver)) b
        c' <- mapM devolve c
        return (a' ++ b' ++ c')

instance ByteCodeStaged ExceptionTable where
  evolveBC f ExceptionTable{..} = label "ExceptionTable" $ do
    catchType <- case catchType of
      0 -> return $ Nothing
      n -> Just <$> link n
    start <- f start
    end <- f end
    handler <- f handler
    return $ ExceptionTable {..}

  devolveBC f ExceptionTable{..} = do
    catchType <- case catchType of
      Just s  -> unlink s
      Nothing -> return $ 0
    start <- f start
    end <- f end
    handler <- f handler
    return $ ExceptionTable {..}


$(deriveBaseWithBinary ''Code)
$(deriveBaseWithBinary ''ExceptionTable)
$(deriveBase ''CodeAttributes)
