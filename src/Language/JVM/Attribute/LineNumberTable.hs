{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module      : Language.JVM.Attribute.LineNumberTable
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the LineNumberTable Attribute,
as documented [here](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.12).
-}

module Language.JVM.Attribute.LineNumberTable
  ( LineNumberTable (..)
  , LineNumber

  , BinaryFormat
  -- * Helper functions
  , linenumber
  , linenumber'
  ) where

import           Control.DeepSeq             (NFData)
import           Data.Binary
import qualified Data.IntMap as IM
import           GHC.Generics                (Generic)
import           Language.JVM.Attribute.Base
import           Language.JVM.Constant
import           Language.JVM.Utils
import           Language.JVM.Stage

-- | 'Signature' is an Attribute.
instance IsAttribute LineNumberTable where
  attrName = Const "LineNumberTable"

type LineNumber = Word16

-- | The 'LineNumberTable' is just a mapping from offsets to linenumbers.
newtype LineNumberTable r = LineNumberTable
  { lineNumberTable :: IM.IntMap LineNumber
  } deriving (Show, Eq, Ord, Generic, NFData)


instance Staged LineNumberTable where
  stage _ (LineNumberTable x) = return $ (LineNumberTable x)

-- | Returns the line number of an offset.
linenumber :: Int -> LineNumberTable r -> Maybe LineNumber
linenumber i (LineNumberTable t) =
  snd <$> IM.lookupLE i t

-- | Returns the line number of an offset. Helper function that also
-- does the conversion from integral to int.
linenumber' :: Integral a => a -> LineNumberTable r -> Maybe LineNumber
linenumber' i = linenumber (fromIntegral i)

type BinaryFormat = SizedList16 (Word16, LineNumber)

instance Binary (LineNumberTable Low) where
  get = do
    LineNumberTable . IM.fromList . map f . unSizedList <$> (get :: Get BinaryFormat)
    where
      f (a,b) = (fromIntegral a, b)
  put (LineNumberTable x) = do
    put sl
    where
      sl :: BinaryFormat
      sl = SizedList . map f . IM.toList $ x
      f (a,b) = (fromIntegral a, b)
