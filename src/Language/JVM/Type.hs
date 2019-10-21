{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE ViewPatterns                #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
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
    ClassName (classNameAsText)
  , textCls
  , strCls
  , dotCls
  , parseClassName
  , serializeClassName

  -- ** JType
  , JType (..)
  , jTypeSize
  , parseJType
  , serializeJType

  , JBaseType (..)
  , jBaseTypeToChar
  , jBaseTypeSize
  , parseJBaseType
  , serializeJBaseType

  , JRefType (..)
  , refTypeDepth
  , parseJRefType
  , serializeJRefType

  , parseFlatJRefType
  , serializeFlatJRefType

  -- ** MethodDescriptor
  , MethodDescriptor (..)
  , parseMethodDescriptor
  , serializeMethodDescriptor

  , ReturnDescriptor (..)
  , parseReturnDescriptor
  , serializeReturnDescriptor

  -- ** FieldDescriptor
  , FieldDescriptor (..)
  , parseFieldDescriptor
  , serializeFieldDescriptor

  -- ** NameAndType
  , NameAndType (..)
  , parseNameAndType
  , serializeNameAndType

  , WithName (..)
  , AsNameAndType (..)

  -- ** MethodId
  , MethodId (..)
  , parseMethodId
  , serializeMethodId

  -- ** FieldId
  , FieldId (..)
  , parseFieldId
  , serializeFieldId

  -- ** InClass
  , InClass (..)
  , parseInClass
  , serializeInClass

  -- ** InRefType
  , InRefType (..)
  , parseInRefType
  , serializeInRefType
  , inRefTypeAsInClass

  -- ** AbsMethodId
  , AbsMethodId (..)
  , parseAbsMethodId
  , serializeAbsMethodId

  -- ** AbsFieldId
  , AbsFieldId (..)
  , parseAbsFieldId
  , serializeAbsFieldId

  -- * Re-export
  , module Language.JVM.TextSerializable
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

-- text
import qualified Data.Text              as Text
import Data.Text.Lazy.Builder as Builder

-- jvm-binary
import Language.JVM.TextSerializable

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let parseTestOne p = parseTest (p <* endOfInput)

-- | A class name
newtype ClassName = ClassName
  { classNameAsText :: Text.Text
  } deriving (Eq, Ord, Generic, NFData)

-- | Parses a ClassName from Text, might fail.
textCls :: Text.Text -> Either String ClassName
textCls = deserialize

-- | Parses a ClassName from String, might fail with an exception.
-- *warning* Unpure.
strCls :: String -> ClassName
strCls = either error id . deserialize . Text.pack

-- | Takes the dot representation and converts it into a class.
dotCls :: Text.Text -> Either String ClassName
dotCls = textCls . Text.map (\c -> if c == '.' then '/' else c)

-- | Parse a 'ClassName', should not be any of '.;[<>:',
--
-- >>> deserialize parseClassName "java/lang/Object"
-- Right "java/lang/Object"
--
-- >>> deserialize parseClassName "java;"
-- Left "endOfInput"
parseClassName :: Parser ClassName
parseClassName =
  ClassName <$> takeWhile1 (notInClass ".;[<>:") <?> "ClassName"

-- | Display a ClassName
serializeClassName :: ClassName -> Builder
serializeClassName =
  Builder.fromText . classNameAsText

instance TextSerializable ClassName where
  parseText = parseClassName
  serialize = serializeClassName

-- | A 'JRefType' is a Class or an Array.
data JRefType
  = JTClass !ClassName
  | JTArray !JType
  deriving (Eq, Ord, Generic, NFData)

-- | The number of nested arrays
refTypeDepth :: JRefType -> Int
refTypeDepth = \case
  JTArray (JTRef a) -> 1 + refTypeDepth a
  JTArray _ -> 1
  JTClass _ -> 0

-- | Parses a 'JRefType'
parseJRefType :: Parser JRefType
parseJRefType = choice
  [ JTArray <$> (char '[' *> parseJType)
  , JTClass <$> (char 'L' *> parseClassName <* char ';')
  ] <?> "JRefType"

serializeJRefType :: JRefType -> Builder
serializeJRefType = \case
  JTArray a -> "[" <> serializeJType a
  JTClass a -> "L" <> serializeClassName a <> ";"

instance TextSerializable JRefType where
  parseText = parseJRefType
  serialize = serializeJRefType

-- | Parses a 'JRefType' but does not require an 'L' infront of
-- the class name, and ';'
-- >>> deserialize parseFlatJRefType "java/lang/Object"
-- Right "Ljava/lang/Object;"
-- >>> deserialize parseFlatJRefType "[I"
-- Right "[I"
parseFlatJRefType :: Parser JRefType
parseFlatJRefType = choice
  [ JTArray <$> (char '[' *> parseJType)
  , JTClass <$> parseClassName
  ] <?> "flat JRefType"

serializeFlatJRefType :: JRefType -> Builder
serializeFlatJRefType = \case
  JTArray a -> "[" <> serializeJType a
  JTClass a -> serializeClassName a

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
  deriving (Eq, Ord, Generic, NFData)

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

-- | Doubles and Longs have size two in the stack.
jBaseTypeSize :: JBaseType -> Int
jBaseTypeSize = \case
  JTDouble -> 2
  JTLong -> 2
  _ -> 1

-- | Parse a JBaseType
parseJBaseType :: Parser JBaseType
parseJBaseType = try . (<?> "JBaseType") $ anyChar >>= \case
  'B' -> return JTByte
  'C' -> return JTChar
  'D' -> return JTDouble
  'F' -> return JTFloat
  'I' -> return JTInt
  'J' -> return JTLong
  'S' -> return JTShort
  'Z' -> return JTBoolean
  s -> fail $ "Unknown char " ++ show s

-- | Serializes JBaseType
serializeJBaseType :: JBaseType -> Builder
serializeJBaseType =
  Builder.singleton . jBaseTypeToChar

instance TextSerializable JBaseType where
  parseText = parseJBaseType
  serialize = serializeJBaseType


-- | A 'JType' is either a simple type or a Reftype
data JType
  = JTBase !JBaseType
  | JTRef !JRefType
  deriving (Eq, Ord, Generic, NFData)

-- | Parse a JType
parseJType :: Parser JType
parseJType = choice
  [ JTRef  <$> parseJRefType
  , JTBase <$> parseJBaseType
  ] <?> "JType"

-- | Serialize 'JType'
serializeJType :: JType -> Builder
serializeJType = \case
  JTRef r  -> serializeJRefType r
  JTBase r -> serializeJBaseType r

instance TextSerializable JType where
  parseText = parseJType
  serialize = serializeJType

-- | jTypes also have different sizes.
jTypeSize :: JType -> Int
jTypeSize = \case
  JTBase a -> jBaseTypeSize a
  JTRef _ -> 1

-- | A ReturnDescriptor is maybe a type, otherwise it is void.
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.3.3
newtype ReturnDescriptor =
  ReturnDescriptor { asMaybeJType :: Maybe JType }
  deriving (Ord, Eq, Generic, NFData)

-- | A ReturnDescriptor is either A JType or A 'void' V annotaiton:
--
-- >>> deserialize parseReturnDescriptor "V"
-- Right Nothing
--
-- >>> parseTest parseReturnDescriptor "[I"
-- Right (Just "[I")
parseReturnDescriptor :: Parser ReturnDescriptor
parseReturnDescriptor = ReturnDescriptor <$> choice
    [ char 'V' >> return Nothing
    , Just <$> parseJType
    ] <?> "return type"

serializeReturnDescriptor :: ReturnDescriptor -> Builder
serializeReturnDescriptor =
  maybe (Builder.singleton 'V') serializeJType
  . asMaybeJType

instance TextSerializable ReturnDescriptor where
  serialize = serializeReturnDescriptor
  parseText = parseReturnDescriptor

-- | Method Descriptor
data MethodDescriptor = MethodDescriptor
  { methodDescriptorArguments  :: ! [JType]
  , methodDescriptorReturnType :: ! ReturnDescriptor
  } deriving (Ord, Eq, Generic, NFData)

-- | A 'MethodDescriptor' is just a list of types
--
-- >>> deserialize parseMethodDescriptor "(II)V"
-- Right "(II)V"
parseMethodDescriptor :: Parser MethodDescriptor
parseMethodDescriptor = (<?> "MethodDescriptor") $ do
  args <- char '(' *> (many' parseJType <?> "method arguments") <* char ')'
  MethodDescriptor args <$> parseReturnDescriptor

serializeMethodDescriptor :: MethodDescriptor -> Builder
serializeMethodDescriptor (MethodDescriptor args rt) =
  singleton '(' <> foldMap serializeJType args <> singleton ')'
  <> serializeReturnDescriptor rt

instance TextSerializable MethodDescriptor where
  serialize = serializeMethodDescriptor
  parseText = parseMethodDescriptor

-- | Field Descriptor
newtype FieldDescriptor = FieldDescriptor
  { fieldDescriptorType :: JType
  } deriving (Ord, Eq, Generic, NFData)

-- | A 'FieldDescriptor' is just a JType
--
-- >>> deserialize parseMethodDescriptor "I"
-- Right "I"
parseFieldDescriptor :: Parser FieldDescriptor
parseFieldDescriptor = (<?> "FieldDescriptor") $ do
  FieldDescriptor <$> parseJType

serializeFieldDescriptor :: FieldDescriptor -> Builder
serializeFieldDescriptor =
   serializeJType . fieldDescriptorType

instance TextSerializable FieldDescriptor where
  parseText = parseFieldDescriptor
  serialize = serializeFieldDescriptor

-- | A name and a type
data NameAndType a = NameAndType !Text.Text !a
  deriving (Show, Eq, Ord, Generic, NFData)

class WithName n where
  type WithNameId n
  (<:>) :: Text.Text -> n -> WithNameId n

class AsNameAndType n where
  type TypeDescriptor n

  toNameAndType :: n -> NameAndType (TypeDescriptor n)

  ntDescriptor :: n -> TypeDescriptor n
  ntDescriptor (toNameAndType -> NameAndType _ d) = d

  ntName :: n -> Text.Text
  ntName (toNameAndType -> NameAndType t _) = t

instance AsNameAndType (NameAndType a) where
  type TypeDescriptor (NameAndType a) = a
  toNameAndType = id


-- | A 'FieldDescriptor' is just a JType
--
-- >>> deserialize (parseNameAndType parseMethodDescriptor) "method:(I)V"
-- Right "method:(I)V"
parseNameAndType :: Parser a -> Parser (NameAndType a)
parseNameAndType parser = (<?> "NameAndType") $ do
    _name <- many1 (notChar ':') <* char ':'
    NameAndType (Text.pack _name) <$> parser

serializeNameAndType :: (a -> Builder) -> NameAndType a -> Builder
serializeNameAndType serializer (NameAndType _name descr) =
  fromText _name <> ":" <> serializer descr

-- | A FieldId
newtype FieldId =
  FieldId { fieldIdAsNameAndType :: NameAndType FieldDescriptor }
  deriving (Ord, Eq, Generic, NFData)

parseFieldId :: Parser FieldId
parseFieldId = FieldId <$> parseNameAndType parseFieldDescriptor

serializeFieldId :: FieldId -> Builder
serializeFieldId = serializeNameAndType serializeFieldDescriptor . fieldIdAsNameAndType

instance TextSerializable FieldId where
  parseText = parseFieldId
  serialize = serializeFieldId

instance WithName FieldDescriptor where
  type WithNameId FieldDescriptor = FieldId
  t <:> mt = FieldId (NameAndType t mt)

instance AsNameAndType FieldId where
  type TypeDescriptor FieldId = FieldDescriptor
  toNameAndType = fieldIdAsNameAndType


-- | A MethodId
newtype MethodId =
  MethodId { methodIdAsNameAndType :: NameAndType MethodDescriptor }
  deriving (Ord, Eq, Generic, NFData)

parseMethodId :: Parser MethodId
parseMethodId = MethodId <$> parseNameAndType parseMethodDescriptor

serializeMethodId :: MethodId -> Builder
serializeMethodId = serializeNameAndType serializeMethodDescriptor . methodIdAsNameAndType

instance TextSerializable MethodId where
  parseText = parseMethodId
  serialize = serializeMethodId

instance WithName MethodDescriptor where
  type WithNameId MethodDescriptor = MethodId
  t <:> mt = MethodId (NameAndType t mt)

instance AsNameAndType MethodId where
  type TypeDescriptor MethodId = MethodDescriptor
  toNameAndType = methodIdAsNameAndType

-- | A method or Field in a Class
data InClass a = InClass
  { inClassName :: !ClassName
  , inClassId :: !a
  } deriving (Eq, Ord, Generic, NFData)

parseInClass :: Parser a -> Parser (InClass a)
parseInClass parseClassId =
  InClass <$> parseClassName <*> (char '.' *> parseClassId)

serializeInClass :: (a -> Builder) -> InClass a -> Builder
serializeInClass serializeClassId (InClass n cid) =
  serializeClassName n <> singleton '.' <> serializeClassId cid

-- | A method or Field in a Class
data InRefType a = InRefType
  { inRefType   :: !JRefType
  , inRefTypeId :: !a
  } deriving (Eq, Ord, Generic, NFData)

parseInRefType :: Parser a -> Parser (InRefType a)
parseInRefType parseRefTypeId =
  InRefType <$> parseJRefType <*> (char '.' *> parseRefTypeId)

serializeInRefType :: (a -> Builder) -> InRefType a -> Builder
serializeInRefType serializeRefTypeId (InRefType n cid) =
  serializeJRefType n <> singleton '.' <> serializeRefTypeId cid

-- | Convert a InRefType to a InClass by casting
-- all arrays to classes.
inRefTypeAsInClass :: InRefType a -> InClass a
inRefTypeAsInClass (InRefType rt rtid) =
  InClass (case rt of JTArray _ -> "java/lang/Object"; JTClass a -> a) rtid

-- | A FieldId
newtype AbsFieldId =
  AbsFieldId { absFieldAsInClass :: InClass FieldId }
  deriving (Ord, Eq, Generic, NFData)

parseAbsFieldId :: Parser AbsFieldId
parseAbsFieldId = AbsFieldId <$> parseInClass parseFieldId

serializeAbsFieldId :: AbsFieldId -> Builder
serializeAbsFieldId = serializeInClass serializeFieldId . absFieldAsInClass

instance TextSerializable AbsFieldId where
  parseText = parseAbsFieldId
  serialize = serializeAbsFieldId

-- | A MethodId
newtype AbsMethodId =
  AbsMethodId { absMethodAsInClass :: InClass MethodId }
  deriving (Ord, Eq, Generic, NFData)

parseAbsMethodId :: Parser AbsMethodId
parseAbsMethodId = AbsMethodId <$> parseInClass parseMethodId

serializeAbsMethodId :: AbsMethodId -> Builder
serializeAbsMethodId = serializeInClass serializeMethodId . absMethodAsInClass

instance TextSerializable AbsMethodId where
  parseText = parseAbsMethodId
  serialize = serializeAbsMethodId

deriveFromTextSerializable ''ClassName
deriveFromTextSerializable ''JType
deriveFromTextSerializable ''JRefType
deriveFromTextSerializable ''JBaseType
deriveFromTextSerializable ''FieldDescriptor
deriveFromTextSerializable ''MethodDescriptor
deriveFromTextSerializable ''ReturnDescriptor

deriveFromTextSerializable ''MethodId
deriveFromTextSerializable ''FieldId

deriveFromTextSerializable ''AbsMethodId
deriveFromTextSerializable ''AbsFieldId

deriving instance Show a => Show (InClass a)
deriving instance Show a => Show (InRefType a)
