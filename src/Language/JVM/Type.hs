{-|
Module      : Language.JVM.Type
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'JType', 'ClassName', 'MethodDescriptor', and
'FieldDescriptor'.
-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
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
  , jBaseTypeP
  , jBaseTypeToChar

  -- ** MethodDescriptor
  , MethodDescriptor (..)

  -- ** FieldDescriptor
  , FieldDescriptor (..)

  -- ** NameAndType
  , NameAndType (..)
  , (<:>)

  , TypeParse (..)
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
  } deriving (Eq, Ord, Generic, NFData)

instance Show ClassName where
  show = show . classNameAsText

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

-- | Method Descriptor
data MethodDescriptor = MethodDescriptor
  { methodDescriptorArguments  :: [JType]
  , methodDescriptorReturnType :: Maybe JType
  } deriving (Show, Ord, Eq, Generic, NFData)

-- | Field Descriptor
newtype FieldDescriptor = FieldDescriptor
  { fieldDescriptorType :: JType
  } deriving (Show, Ord, Eq, Generic, NFData)

-- | A name and a type
data NameAndType a = NameAndType
  { ntName       :: Text.Text
  , ntDescriptor :: a
  } deriving (Show, Eq, Ord, Generic, NFData)

(<:>) :: Text.Text -> a -> NameAndType a
(<:>) = NameAndType

class TypeParse a where
  fromText :: Text.Text -> Either String a
  fromText = parseOnly (parseText <* endOfInput)
  parseText :: Parser a
  toText :: a -> Text.Text

jBaseTypeP :: Parser JBaseType
jBaseTypeP = do
  s <- satisfy (inClass "BCDFIJSZ") <?> "BaseType"
  case s of
    'B' -> return $ JTByte
    'C' -> return $ JTChar
    'D' -> return $ JTDouble
    'F' -> return $ JTFloat
    'I' -> return $ JTInt
    'J' -> return $ JTLong
    'S' -> return $ JTShort
    'Z' -> return $ JTBoolean
    _ -> error "should not happen"

jBaseTypeToChar :: JBaseType -> Char
jBaseTypeToChar y = do
  case y of
    JTByte    -> 'B'
    JTChar    -> 'C'
    JTDouble  -> 'D'
    JTFloat   -> 'F'
    JTInt     -> 'I'
    JTLong    -> 'J'
    JTShort   -> 'S'
    JTBoolean -> 'Z'

instance TypeParse JType where
  parseText = do
    choice
      [ JTBase <$> jBaseTypeP
      , do
          _ <- char 'L'
          txt <- takeWhile (/= ';')
          _ <- char ';'
          return $ JTClass (ClassName txt)
      , char '[' >> JTArray <$> parseText
      ]
  toText tp =
    Text.pack $ go tp ""
    where
      go x =
        case x of
          JTBase y               -> (jBaseTypeToChar y :)
          JTClass (ClassName cn) -> ((('L':Text.unpack cn) ++ ";") ++)
          JTArray tp'            -> ('[':) . go tp'

instance TypeParse MethodDescriptor where
  toText md =
    Text.concat (
      ["("]
      ++ map toText (methodDescriptorArguments md)
      ++ [")", maybe "V" toText $ methodDescriptorReturnType md ]
    )
  parseText = do
    _ <- char '('
    args <- many' parseText <?> "method arguments"
    _ <- char ')'
    returnType <- choice
      [ char 'V' >> return Nothing
      , Just <$> parseText
      ] <?> "return type"
    return $ MethodDescriptor args returnType

instance TypeParse FieldDescriptor where
  parseText = FieldDescriptor <$> parseText
  toText (FieldDescriptor t) = toText t

instance TypeParse t => TypeParse (NameAndType t)  where
  parseText = do
    name <- many1 $ notChar ':'
    _ <- char ':'
    _type <- parseText
    return $ NameAndType (Text.pack name) _type
  toText (NameAndType name _type) =
    Text.concat [ name , ":" , toText _type ]

fromString' ::
  TypeParse t
  => String
  -> t
fromString' =
  either (error . ("Failed " ++)) id . fromText . Text.pack

instance IsString ClassName where
  fromString = strCls

instance IsString JType where
  fromString = fromString'

instance IsString FieldDescriptor where
  fromString = fromString'

instance IsString MethodDescriptor where
  fromString = fromString'

instance TypeParse t => IsString (NameAndType t) where
  fromString = fromString'
