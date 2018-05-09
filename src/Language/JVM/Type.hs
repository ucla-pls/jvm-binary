{-|
Module      : Language.JVM.Type
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'JType'.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Language.JVM.Type
  ( ClassName (..)
  , strCls

  , JType (..)
  , jTypeToText

  , JBaseType (..)

  , MethodDescriptor (..)
  , methodDescriptorFromText
  , methodDescriptorToText

  , FieldDescriptor (..)
  , fieldDescriptorFromText
  , fieldDescriptorToText

  -- * Parsers
  , parseJType
  , parseMethodDescriptor
  , parseFieldDescriptor
  ) where

import           Control.DeepSeq      (NFData)
import           Data.Attoparsec.Text
import qualified Data.Text            as Text
import           GHC.Generics         (Generic)
import           Prelude              hiding (takeWhile)

-- | A class name
newtype ClassName = ClassName
  { classNameAsText :: Text.Text
  } deriving (Show, Eq, Ord, Generic, NFData)

-- | Class cls
strCls :: String -> ClassName
strCls = ClassName . Text.pack

-- | A Jvm a base type
data JBaseType
  = JTByte
  | JTChar
  | JTDouble
  | JTFloat
  | JTInt
  | JTLong
  | JTShort
  | JTBoolean
  deriving (Show, Eq, Ord, Generic, NFData)

-- | A Jvm primitive type
data JType
  = JTBase JBaseType
  | JTClass ClassName
  | JTArray JType
  deriving (Show, Eq, Ord, Generic, NFData)

-- | Method Descriptor
data MethodDescriptor = MethodDescriptor
  { methodDescriptorArguments  :: [JType]
  , methodDescriptorReturnType :: Maybe JType
  } deriving (Show, Ord, Eq, Generic, NFData)

-- | Field Descriptor
newtype FieldDescriptor = FieldDescriptor
  { fieldDescriptorType :: JType
  } deriving (Show, Ord, Eq, Generic, NFData)

-- | Parse a JType
parseJType :: Parser JType
parseJType = try $ do
  s <- anyChar
  case s :: Char of
    'B' -> return $ JTBase JTByte
    'C' -> return $ JTBase JTChar
    'D' -> return $ JTBase JTDouble
    'F' -> return $ JTBase JTFloat
    'I' -> return $ JTBase JTInt
    'J' -> return $ JTBase JTLong
    'L' -> do
      txt <- takeWhile (/= ';')
      _ <- char ';'
      return $ JTClass (ClassName txt)
    'S' -> return $ JTBase JTShort
    'Z' -> return $ JTBase JTBoolean
    '[' -> JTArray <$> parseJType
    _ -> fail $ "Unknown char " ++ show s

jTypeToText :: JType -> Text.Text
jTypeToText tp =
  Text.pack $ go tp ""
  where
    go x =
      case x of
        JTBase JTByte -> ('B':)
        JTBase JTChar -> ('C':)
        JTBase JTDouble -> ('D':)
        JTBase JTFloat -> ('F':)
        JTBase JTInt -> ('I':)
        JTBase JTLong -> ('J':)
        JTClass (ClassName cn) -> ((('L':Text.unpack cn) ++ ";") ++)
        JTBase JTShort -> ('S':)
        JTBase JTBoolean -> ('Z':)
        JTArray tp' -> ('[':) . go tp'


methodDescriptorToText :: MethodDescriptor -> Text.Text
methodDescriptorToText md =
  Text.concat $
    ["("] ++ map jTypeToText (methodDescriptorArguments md)
    ++ [")", case methodDescriptorReturnType md of
               Just x -> jTypeToText x
               Nothing -> "V"
           ]

fieldDescriptorToText :: FieldDescriptor -> Text.Text
fieldDescriptorToText (FieldDescriptor jtp) =
  jTypeToText jtp

-- | Parse a method descriptor
parseMethodDescriptor :: Parser MethodDescriptor
parseMethodDescriptor = do
  _ <- char '('
  args <- (many' $ parseJType) <?> "method arguments"
  _ <- char ')'
  returnType <- choice
    [ char 'V' >> return Nothing
    , Just <$> parseJType
    ] <?> "return type"
  return $ MethodDescriptor args returnType

-- | Read a method descriptor from `Text.Text`
methodDescriptorFromText :: Text.Text -> Either String MethodDescriptor
methodDescriptorFromText = parseOnly parseMethodDescriptor

-- | Parse a field descriptor
parseFieldDescriptor :: Parser FieldDescriptor
parseFieldDescriptor = FieldDescriptor <$> parseJType

-- | Read a field descriptor from `Text.Text`.
fieldDescriptorFromText :: Text.Text -> Either String FieldDescriptor
fieldDescriptorFromText = parseOnly parseFieldDescriptor
