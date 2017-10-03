{-|
Module      : Language.JVM.Type
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'JType'.
-}

module Language.JVM.Type
  ( ClassName (..)
  , JType (..)

  , MethodDescriptor (..)
  , FieldDescriptor (..)

  -- * Parsers
  , parseJType

  , parseMethodDescriptor
  , methodDescriptorFromText
  , parseFieldDescriptor
  , fieldDescriptorFromText
  ) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as Text

-- | A class name
newtype ClassName = ClassName
  { classNameAsText :: Text.Text
  } deriving (Show, Eq, Ord)

-- | A Jvm primitive type
data JType
  = JTByte
  | JTChar
  | JTDouble
  | JTFloat
  | JTInt
  | JTLong
  | JTClass ClassName
  | JTShort
  | JTBoolean
  | JTArray JType
  deriving (Show, Eq, Ord)

-- | Method Descriptor
data MethodDescriptor = MethodDescriptor
  { methodArguments :: [JType]
  , methodReturnType :: Maybe JType
  } deriving (Show, Ord, Eq)

-- | Field Descriptor
newtype FieldDescriptor = FieldDescriptor
  { fieldType :: JType
  } deriving (Show, Ord, Eq)

type Parser = Parsec Void Text.Text

-- | Parse a JType
parseJType :: Parser JType
parseJType = try $ do
  s <- anyChar
  case s :: Char of
    'B' -> return JTByte
    'C' -> return JTChar
    'D' -> return JTDouble
    'F' -> return JTFloat
    'I' -> return JTInt
    'J' -> return JTLong
    'L' -> do
      txt <- takeWhileP (Just "ClassName") (/= ';')
      _ <- char ';'
      return $ JTClass (ClassName txt)
    'S' -> return JTShort
    'Z' -> return JTBoolean
    '[' -> JTArray <$> parseJType
    _ -> fail $ "Unknown char " ++ show s

-- | Parse a method descriptor
parseMethodDescriptor :: Parser MethodDescriptor
parseMethodDescriptor = do
  _ <- char '('
  args <- (many $ parseJType) <?> "method arguments"
  _ <- char ')'
  returnType <- choice
    [ char 'V' >> return Nothing
    , Just <$> parseJType
    ] <?> "return type"
  return $ MethodDescriptor args returnType

-- | Read a method descriptor from `Text.Text`
methodDescriptorFromText :: Text.Text -> Maybe MethodDescriptor
methodDescriptorFromText = parseMaybe (parseMethodDescriptor <* eof)

-- | Parse a field descriptor
parseFieldDescriptor :: Parser FieldDescriptor
parseFieldDescriptor = FieldDescriptor <$> parseJType

-- | Read a field descriptor from `Text.Text`.
fieldDescriptorFromText :: Text.Text -> Maybe FieldDescriptor
fieldDescriptorFromText = parseMaybe (parseFieldDescriptor <* eof)
