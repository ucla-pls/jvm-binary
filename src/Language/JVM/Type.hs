{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Language.JVM.Type
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the 'JType', 'ClassName', 'MethodDescriptor', and
'FieldDescriptor'.
-}
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
  , jBaseTypeToChar

  , JRefType (..)
  , refTypeDepth

  -- ** MethodDescriptor
  , MethodDescriptor (..)

  -- ** FieldDescriptor
  , FieldDescriptor (..)

  -- ** NameAndType
  , NameAndType (..)
  , (<:>)

  -- * TypeParse
  , TypeParse (..)
  , typeFromText
  , typeToText
  , parseOnly

  , parseFlatJRefType
  , jRefTypeToFlatText
  ) where

-- base
import           Data.String
import           Control.Applicative
import           Data.Semigroup
import           GHC.Generics           (Generic)
import           Prelude                hiding (takeWhile)

-- deepseq
import           Control.DeepSeq        (NFData)

-- attoparsec
import           Data.Attoparsec.Text

-- mtl
import           Control.Monad.Writer hiding ((<>))

-- text
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as Lazy
import qualified Data.Text.Lazy.Builder as Builder


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

data JRefType
  = JTClass !ClassName
  | JTArray !JType
  deriving (Show, Eq, Ord, Generic, NFData)

-- | The number of nested arrays
refTypeDepth :: JRefType -> Int
refTypeDepth = \case
  JTArray (JTRef a) -> 1 + (refTypeDepth a)
  JTArray _ -> 1
  JTClass _ -> 0

data JType
  = JTBase JBaseType
  | JTRef JRefType
  deriving (Show, Eq, Ord, Generic, NFData)


-- | Get the corresponding `Char` of a `JBaseType`
jBaseTypeToChar :: JBaseType -> Char
jBaseTypeToChar = \case
    JTByte    -> 'B'
    JTChar    -> 'C'
    JTDouble  -> 'D'
    JTFloat   -> 'F'
    JTInt     -> 'I'
    JTLong    -> 'J'
    JTShort   -> 'S'
    JTBoolean -> 'Z'

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
  -- | A `TypeParse` should be parsable
  parseType :: Parser a

  -- | A `TypeParse` should be printable
  typeToBuilder :: a -> Builder.Builder

-- | Parse a type from text
typeFromText :: TypeParse a => Text.Text -> Either String a
typeFromText = parseOnly (parseType <* endOfInput)

-- | Convert a type into text
typeToText :: TypeParse a => a -> Text.Text
typeToText = Lazy.toStrict . Builder.toLazyText . typeToBuilder

instance TypeParse ClassName where
  parseType = ClassName <$> takeWhile1 (notInClass ".;[<>:") <?> "ClassName"
  typeToBuilder = Builder.fromText . classNameAsText

instance TypeParse JBaseType where
  parseType = try . (<?> "BaseType") $ do
    anyChar >>= \case
      'B' -> return $ JTByte
      'C' -> return $ JTChar
      'D' -> return $ JTDouble
      'F' -> return $ JTFloat
      'I' -> return $ JTInt
      'J' -> return $ JTLong
      'S' -> return $ JTShort
      'Z' -> return $ JTBoolean
      s -> fail $ "Unknown char " ++ show s

  typeToBuilder = Builder.singleton . jBaseTypeToChar

instance TypeParse JRefType where
  parseType = try . (<?> "RefType") $ do
    anyChar >>= \case
      'L' -> do
        txt <- takeWhile (/= ';')
        _ <- char ';'
        return $ JTClass (ClassName txt)
      '[' -> JTArray <$> parseType
      s -> fail $ "Unknown char " ++ show s

  typeToBuilder = \case
    JTClass cn ->
      Builder.singleton 'L' <> typeToBuilder cn <> Builder.singleton ';'
    JTArray t -> do
      Builder.singleton '[' <> typeToBuilder t

parseFlatJRefType :: Parser (JRefType)
parseFlatJRefType =
  JTArray <$> (char '[' *> parseType)
  <|> JTClass <$> parseType

jRefTypeToFlatText :: JRefType -> Text.Text
jRefTypeToFlatText = \case
  JTClass t' -> classNameAsText t'
  JTArray t' -> Lazy.toStrict . Builder.toLazyText $ Builder.singleton '[' <> typeToBuilder t'

instance TypeParse JType where
  parseType =
    (JTRef <$> parseType <|> JTBase <$> parseType)
    <?> "JType"

  typeToBuilder = \case
    JTRef r  -> typeToBuilder r
    JTBase r -> typeToBuilder r

instance TypeParse MethodDescriptor where
  typeToBuilder md =
    execWriter $ do
      tell $ Builder.singleton '('
      mapM_ (tell . typeToBuilder) (methodDescriptorArguments md)
      tell $ Builder.singleton ')'
      tell . maybe (Builder.singleton 'V') typeToBuilder $ methodDescriptorReturnType md

  parseType = do
    _ <- char '('
    args <- many' parseType <?> "method arguments"
    _ <- char ')'
    returnType <- choice
      [ char 'V' >> return Nothing
      , Just <$> parseType
      ] <?> "return type"
    return $ MethodDescriptor args returnType

instance TypeParse FieldDescriptor where
  parseType = FieldDescriptor <$> parseType
  typeToBuilder (FieldDescriptor t) = typeToBuilder t

instance TypeParse t => TypeParse (NameAndType t)  where

  parseType = do
    name <- many1 $ notChar ':'
    _ <- char ':'
    _type <- parseType
    return $ NameAndType (Text.pack name) _type

  typeToBuilder (NameAndType name _type) =
    Builder.fromText name
    <> Builder.singleton ':'
    <> typeToBuilder _type

fromString' ::
  TypeParse t
  => String
  -> t
fromString' =
  either (error . ("Failed " ++)) id . typeFromText . Text.pack

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
