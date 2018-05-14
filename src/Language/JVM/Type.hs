{-|
Module      : Language.JVM.Type
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'JType', 'ClassName', 'MethodDescriptor', and 'FieldDescriptor'.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Language.JVM.Type
  (
  -- * Base types
  -- ** ClassName
    ClassName (..)
  , strCls
  , dotCls

  -- ** JType
  , JType (..)
  , JBaseType (..)
  , jTypeFromText
  , jTypeToText
  , parseJType

  -- * MethodDescriptor
  , MethodDescriptor (..)
  , methodDescriptorFromText
  , methodDescriptorToText
  , parseMethodDescriptor

  -- * FieldDescriptor
  , FieldDescriptor (..)
  , fieldDescriptorFromText
  , fieldDescriptorToText
  , parseFieldDescriptor

  ) where

import           Control.DeepSeq      (NFData)
import           Data.Attoparsec.Text
import           Data.String
import qualified Data.Text            as Text
import           GHC.Generics         (Generic)
import           Prelude              hiding (takeWhile)

-- | A class name
newtype ClassName = ClassName
  { classNameAsText :: Text.Text
  } deriving (Show, Eq, Ord, Generic, NFData)


-- | Wrapper method that converts a string representation of a class into
-- a class. 
strCls :: String -> ClassName
strCls = dotCls . Text.pack

-- | Takes the dot representation and converts it into a class. 
dotCls :: Text.Text -> ClassName
dotCls = ClassName . Text.intercalate "/" . Text.splitOn "."

-- | The Jvm Primitive Types
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

-- | The JVM types
data JType
  = JTBase JBaseType
  | JTClass ClassName
  | JTArray JType
  deriving (Show, Eq, Ord, Generic, NFData)

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

-- | Read a method descriptor from `Text.Text`
jTypeFromText :: Text.Text -> Either String JType
jTypeFromText = parseOnly parseJType

-- | Converts a 'JType' into 'Text.Text'. It follows the scheme of
-- the jvm descriptor <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3>.
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


-- | Method Descriptor
data MethodDescriptor = MethodDescriptor
  { methodDescriptorArguments  :: [JType]
  , methodDescriptorReturnType :: Maybe JType
  } deriving (Show, Ord, Eq, Generic, NFData)

-- | Field Descriptor
newtype FieldDescriptor = FieldDescriptor
  { fieldDescriptorType :: JType
  } deriving (Show, Ord, Eq, Generic, NFData)

-- | Convert a 'MethodDescriptor' to 'Text.Text'
methodDescriptorToText :: MethodDescriptor -> Text.Text
methodDescriptorToText md =
  Text.concat $
    ["("] ++ map jTypeToText (methodDescriptorArguments md)
    ++ [")", case methodDescriptorReturnType md of
               Just x -> jTypeToText x
               Nothing -> "V"
           ]

-- | Convert a 'FieldDescriptor' to 'Text.Text'
fieldDescriptorToText :: FieldDescriptor -> Text.Text
fieldDescriptorToText (FieldDescriptor jtp) =
  jTypeToText jtp

-- | Parse a method descriptor
parseMethodDescriptor :: Parser MethodDescriptor
parseMethodDescriptor = do
  _ <- char '('
  args <- many' parseJType <?> "method arguments"
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

instance IsString ClassName where
  fromString = strCls

instance IsString JType where
  fromString = either (error . ("Failed " ++)) id . jTypeFromText . Text.pack

instance IsString MethodDescriptor where
  fromString = either (error . ("Failed " ++)) id . methodDescriptorFromText . Text.pack

instance IsString FieldDescriptor where
  fromString = either (error . ("Failed " ++)) id . fieldDescriptorFromText . Text.pack


