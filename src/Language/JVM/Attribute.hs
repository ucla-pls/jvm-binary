{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module      : Language.JVM.Attribute
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This is the main module for accessing all kinds of Attributes.
-}
module Language.JVM.Attribute (
  module Language.JVM.Attribute.Base,

  -- * Subattributes
  BootstrapMethods,
  Code,
  ConstantValue,
  Exceptions,
  LineNumberTable,
  StackMapTable,
  Signature,
  RuntimeVisibleAnnotations,
  RuntimeInvisibleAnnotations,
  RuntimeVisibleParameterAnnotations,
  RuntimeInvisibleParameterAnnotations,
  RuntimeVisibleTypeAnnotations,
  RuntimeInvisibleTypeAnnotations,
  ClassTypeAnnotation,
  MethodTypeAnnotation,
  FieldTypeAnnotation,
  CodeTypeAnnotation,
  AnnotationDefault,
  MethodParameters,
)
where

import Language.JVM.Attribute.Annotations (
  AnnotationDefault,
  ClassTypeAnnotation,
  CodeTypeAnnotation,
  FieldTypeAnnotation,
  MethodTypeAnnotation,
  RuntimeInvisibleAnnotations,
  RuntimeInvisibleParameterAnnotations,
  RuntimeInvisibleTypeAnnotations,
  RuntimeVisibleAnnotations,
  RuntimeVisibleParameterAnnotations,
  RuntimeVisibleTypeAnnotations,
 )
import Language.JVM.Attribute.Base
import Language.JVM.Attribute.BootstrapMethods (
  BootstrapMethods,
 )
import Language.JVM.Attribute.Code (Code)
import Language.JVM.Attribute.ConstantValue (
  ConstantValue,
 )
import Language.JVM.Attribute.Exceptions (
  Exceptions,
 )
import Language.JVM.Attribute.LineNumberTable (
  LineNumberTable,
 )
import Language.JVM.Attribute.MethodParameters (
  MethodParameters,
 )
import Language.JVM.Attribute.Signature (
  Signature,
 )
import Language.JVM.Attribute.StackMapTable (
  StackMapTable,
 )
