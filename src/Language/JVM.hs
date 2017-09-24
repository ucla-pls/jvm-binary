{-|
Module      : Language.JVM
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

The main entry point for using the library.
-}

module Language.JVM
  ( decodeClassFile

  , module Language.JVM.Attribute
  , module Language.JVM.ClassFile
  , module Language.JVM.Constant
  , module Language.JVM.Field
  , module Language.JVM.Method
  ) where

import qualified Data.ByteString.Lazy   as BL
import           Data.Binary

import           Language.JVM.Attribute
import           Language.JVM.ClassFile
import           Language.JVM.Constant
import           Language.JVM.Field
import           Language.JVM.Method

-- | Create a class file from a lazy 'BL.ByteString'
decodeClassFile :: BL.ByteString -> Either String ClassFile
decodeClassFile bs = do
  case decodeOrFail bs of
    Right (_, _, cf) -> Right cf
    Left (_, _, msg) -> Left msg

