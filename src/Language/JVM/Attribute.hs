{-|
Module      : Language.JVM.Attribute
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This is the main module for accessing all kinds of Attributes.
-}

{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.JVM.Attribute
  ( module Language.JVM.Attribute.Base
  -- * Subattributes
  , BootstrapMethods
  , Code
  , ConstantValue
  , Exceptions
  , LineNumberTable
  , StackMapTable
  ) where

import           Language.JVM.Attribute.Base
import           Language.JVM.Attribute.BootstrapMethods (BootstrapMethods)
import           Language.JVM.Attribute.Code             (Code)
import           Language.JVM.Attribute.ConstantValue    (ConstantValue)
import           Language.JVM.Attribute.Exceptions       (Exceptions)
import           Language.JVM.Attribute.LineNumberTable  (LineNumberTable)
import           Language.JVM.Attribute.StackMapTable    (StackMapTable)
