{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module      : Language.JVM.Attribute.Signature
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the Signature Attribute,
as documented [here](http://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.7.9),
and the signature syntax defined [here](https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.7.9.1).
-}

module Language.JVM.Attribute.Signature
  ( Signature(..)
  , signatureToText
  , signatureFromText

  -- * Top Level Definitions
  , ClassSignature(..)
  , isSimpleClassSignature
  , classSignatureToText
  , classSignatureFromText
  , MethodSignature(..)
  , isSimpleMethodSignature
  , methodSignatureToText
  , methodSignatureFromText
  , FieldSignature(..)
  , isSimpleFieldSignature
  , fieldSignatureToText
  , fieldSignatureFromText

  -- ** Handlers


  -- * Lower Level Definitions
  , ClassType(..)
  , isSimpleClassType
  , classTypeToName
  , classTypeFromName
  , InnerClassType(..)
  , ReferenceType(..)
  , isSimpleReferenceType
  , referenceTypeFromRefType
  , ThrowsSignature(..)
  , isSimpleThrowsSignature
  , throwsSignatureFromName
  , TypeSignature(..)
  , isSimpleTypeSignature
  , typeSignatureFromType
  , TypeArgument(..)
  , TypeParameter(..)
  , TypeVariable(..)
  , Wildcard(..)

  -- * Parsers
  , classSignatureP
  , methodSignatureP
  , fieldSignatureP
  , classTypeP
  , classTypeT
  , referenceTypeP
  , referenceTypeT
  , throwsSignatureP
  , throwsSignatureT
  , typeArgumentsT
  , typeArgumentsP
  , typeArgumentP
  , typeArgumentT
  , typeParameterP
  , typeParameterT
  , typeParametersT
  , typeParametersP
  , typeSignatureP
  , typeSignatureT
  , typeVariableP
  )
where

import           Control.DeepSeq                ( NFData )
import qualified Data.Text                     as Text

import qualified Data.Text.Lazy                as LText
import           Data.Text.Lazy.Builder        as Text

import           Data.Functor
import           GHC.Generics                   ( Generic )

import           Data.Attoparsec.Text
import           Control.Applicative

import qualified Data.List                     as L

import           Language.JVM.Attribute.Base
import           Language.JVM.Staged
import           Language.JVM.Type

instance IsAttribute (Signature Low) where
  attrName = Const "Signature"

newtype Signature a =
  Signature (Ref Text.Text a)

signatureToText :: Signature High -> Text.Text
signatureToText (Signature s) = s

signatureFromText :: Text.Text -> Signature High
signatureFromText = Signature

data ClassSignature = ClassSignature
  { csTypeParameters      :: [TypeParameter]
  , csSuperclassSignature :: ClassType
  , csInterfaceSignatures :: [ClassType]
  }
  deriving (Show, Eq, Ord, Generic, NFData)

data MethodSignature = MethodSignature
  { msTypeParameters :: [TypeParameter]
  , msArguments      :: [TypeSignature]
  , msResults        :: Maybe TypeSignature
  , msThrows         :: [ ThrowsSignature ]
  }
  deriving (Show, Eq, Ord, Generic, NFData)

newtype FieldSignature =
  FieldSignature {fsRefType :: ReferenceType}
  deriving (Show, Eq, Ord, Generic, NFData)

data TypeSignature
  = ReferenceType ReferenceType
  | BaseType JBaseType
  deriving (Show, Eq, Ord, Generic, NFData)

data ReferenceType
  = RefClassType ClassType
  | RefTypeVariable TypeVariable
  | RefArrayType TypeSignature
  deriving (Show, Eq, Ord, Generic, NFData)

data ClassType
  = ClassType
    { ctsName          :: !ClassName
    , ctsInnerClass    :: !(Maybe InnerClassType)
    , ctsTypeArguments :: [Maybe TypeArgument]
    }
  deriving (Show, Eq, Ord, Generic, NFData)

data InnerClassType
  = InnerClassType
    { ictsName          :: !Text.Text
    , ictsInnerClass    :: !(Maybe InnerClassType)
    , ictsTypeArguments :: [Maybe TypeArgument]
    }
  deriving (Show, Eq, Ord, Generic, NFData)

data TypeArgument = TypeArgument
  { taWildcard :: Maybe Wildcard
  , taType     :: ReferenceType
  } deriving (Show, Eq, Ord, Generic, NFData)

data Wildcard =
  WildPlus | WildMinus
  deriving (Show, Eq, Ord, Generic, NFData)

newtype TypeVariable =
  TypeVariable { tvAsText :: Text.Text }
  deriving (Show, Eq, Ord, Generic, NFData)

data TypeParameter =
  TypeParameter
  { tpIdentifier    :: Text.Text
  , tpClassBound     :: Maybe ReferenceType
  , tpInterfaceBound :: [ReferenceType]
  }
  deriving (Show, Eq, Ord, Generic, NFData)

data ThrowsSignature
  = ThrowsClass ClassType
  | ThrowsTypeVariable TypeVariable
  deriving (Show, Eq, Ord, Generic, NFData)

-- Conversion

classTypeToName :: ClassType -> ClassName
classTypeToName =
  (either error id . textCls . Text.intercalate "$" . getClassName)
 where
  getClassName (ClassType {..}) =
    classNameAsText ctsName : getInnerClassName ctsInnerClass

  getInnerClassName = \case
    Just (InnerClassType {..}) -> ictsName : getInnerClassName ictsInnerClass
    Nothing                    -> []

-- | Create a classType from a Name
-- Note the language is wierd here! Main.A is not Main$A, but Main<T>.A is!
classTypeFromName :: ClassName -> ClassType
classTypeFromName cn = ClassType cn Nothing []

throwsSignatureFromName :: ClassName -> ThrowsSignature
throwsSignatureFromName cn = ThrowsClass (classTypeFromName cn)

referenceTypeFromRefType :: JRefType -> ReferenceType
referenceTypeFromRefType = \case
  JTArray a -> RefArrayType (typeSignatureFromType a)
  JTClass a -> RefClassType (classTypeFromName a)

typeSignatureFromType :: JType -> TypeSignature
typeSignatureFromType = \case
  JTBase a -> BaseType a
  JTRef  a -> ReferenceType (referenceTypeFromRefType a)

isSimpleMethodSignature :: MethodSignature -> Bool
isSimpleMethodSignature MethodSignature {..} = and
  [ null msTypeParameters
  , all isSimpleTypeSignature   msArguments
  , all isSimpleTypeSignature   msResults
  , all isSimpleThrowsSignature msThrows
  ]

isSimpleClassSignature :: ClassSignature -> Bool
isSimpleClassSignature ClassSignature {..} = and
  [ null csTypeParameters
  , isSimpleClassType csSuperclassSignature
  , all isSimpleClassType csInterfaceSignatures
  ]

isSimpleFieldSignature :: FieldSignature -> Bool
isSimpleFieldSignature FieldSignature {..} = isSimpleReferenceType fsRefType

isSimpleTypeSignature :: TypeSignature -> Bool
isSimpleTypeSignature = \case
  BaseType      _ -> True
  ReferenceType a -> isSimpleReferenceType a

isSimpleReferenceType :: ReferenceType -> Bool
isSimpleReferenceType = \case
  RefArrayType    a -> isSimpleTypeSignature a
  RefClassType    a -> isSimpleClassType a
  RefTypeVariable _ -> False

isSimpleClassType :: ClassType -> Bool
isSimpleClassType = \case
  ClassType _ Nothing [] -> True
  _                      -> False

isSimpleThrowsSignature :: ThrowsSignature -> Bool
isSimpleThrowsSignature = \case
  ThrowsClass        a -> isSimpleClassType a
  ThrowsTypeVariable _ -> False



instance TextSerializable ClassSignature where
  parseText = classSignatureP
  toBuilder = classSignatureT

instance TextSerializable MethodSignature where
  parseText = methodSignatureP
  toBuilder = methodSignatureT

instance TextSerializable FieldSignature where
  parseText = fieldSignatureP
  toBuilder = fieldSignatureT

instance TextSerializable TypeSignature where
  parseText = typeSignatureP
  toBuilder = typeSignatureT

instance TextSerializable ReferenceType where
  parseText = referenceTypeP
  toBuilder = referenceTypeT

instance TextSerializable ClassType where
  parseText = classTypeP
  toBuilder = classTypeT

instance TextSerializable Wildcard where
  parseText = wildcardP
  toBuilder = wildcardT

instance TextSerializable TypeVariable where
  parseText = typeVariableP
  toBuilder = typeVariableT

instance TextSerializable TypeParameter where
  parseText = typeParameterP
  toBuilder = typeParameterT

instance TextSerializable ThrowsSignature where
  parseText = throwsSignatureP
  toBuilder = throwsSignatureT

----------------------
-- Parsing
----------------------

classSignatureP :: Parser ClassSignature
classSignatureP = do
  tp <- option [] typeParametersP
  ss <- classTypeP
  is <- many' classTypeP
  return $ ClassSignature tp ss is

classSignatureToText :: ClassSignature -> Text.Text
classSignatureToText = LText.toStrict . toLazyText . classSignatureT

classSignatureFromText :: Text.Text -> Either String ClassSignature
classSignatureFromText = parseOnly classSignatureP

classSignatureT :: ClassSignature -> Builder
classSignatureT (ClassSignature tp ct its) = do
  typeParametersT tp <> foldMap classTypeT (ct : its)


typeSignatureP :: Parser TypeSignature
typeSignatureP = do
  choice
    [ (ReferenceType <$> referenceTypeP) <?> "JRefereceType"
    , (BaseType <$> parseJBaseType) <?> "JBaseType"
    ]

typeSignatureT :: TypeSignature -> Builder
typeSignatureT (ReferenceType t) = referenceTypeT t
typeSignatureT (BaseType      t) = singleton (jBaseTypeToChar t)

referenceTypeP :: Parser ReferenceType
referenceTypeP = do
  choice
    [ RefClassType <$> classTypeP
    , RefTypeVariable <$> typeVariableP
    , RefArrayType <$> (char '[' >> typeSignatureP)
    ]

referenceTypeT :: ReferenceType -> Builder
referenceTypeT t = case t of
  RefClassType    ct -> classTypeT ct
  RefTypeVariable tv -> typeVariableT tv
  RefArrayType    at -> singleton '[' <> typeSignatureT at


classTypeP :: Parser ClassType
classTypeP = nameit "ClassType" $ do
  _   <- char 'L'
  cn  <- parseClassName
  ta  <- option [] typeArgumentsP
  ict <- many' $ do
    _   <- char '.'
    i   <- identifierP
    ta' <- option [] typeArgumentsP
    return (i, ta')
  _ <- char ';'
  return $ ClassType
    cn
    (L.foldr (\(i, ta') a -> Just $ InnerClassType i a ta') Nothing ict)
    ta

classTypeT :: ClassType -> Builder
classTypeT (ClassType n ic arg) =
  singleton 'L'
    <> Text.fromText (classNameAsText n)
    <> typeArgumentsT arg
    <> go ic
    <> singleton ';'
 where
  go = \case
    Nothing -> mempty
    Just (InnerClassType n' ic' arg') ->
      singleton '.' <> Text.fromText n' <> typeArgumentsT arg' <> go ic'


typeArgumentsP :: Parser [Maybe TypeArgument]
typeArgumentsP = do
  _   <- char '<'
  tas <- many1' typeArgumentP
  _   <- char '>'
  return tas

typeArgumentP :: Parser (Maybe TypeArgument)
typeArgumentP = do
  choice
      [ Just
        <$> (   TypeArgument
            <$> option Nothing (Just <$> wildcardP)
            <*> referenceTypeP
            )
      , char '*' $> Nothing
      ]
    <?> "TypeArgument"

typeArgumentsT :: [Maybe TypeArgument] -> Builder
typeArgumentsT args = do
  if L.null args
    then mempty
    else singleton '<' <> foldMap typeArgumentT args <> singleton '>'

typeArgumentT :: Maybe TypeArgument -> Builder
typeArgumentT a = do
  case a of
    Nothing -> singleton '*'
    Just (TypeArgument w rt) ->
      (case w of
          Just m  -> wildcardT m
          Nothing -> mempty
        )
        <> referenceTypeT rt


wildcardP :: Parser Wildcard
wildcardP = choice [char '+' $> WildPlus, char '-' $> WildMinus]

wildcardT :: Wildcard -> Builder
wildcardT = \case
  WildPlus  -> singleton '+'
  WildMinus -> singleton '-'


typeVariableP :: Parser TypeVariable
typeVariableP = do
  _ <- char 'T'
  t <- identifierP
  _ <- char ';'
  return $ TypeVariable t

typeVariableT :: TypeVariable -> Builder
typeVariableT (TypeVariable t) = do
  singleton 'T' <> Text.fromText t <> singleton ';'


typeParametersP :: Parser [TypeParameter]
typeParametersP = nameit "TypeParameters" $ do
  _   <- char '<'
  tps <- many1' typeParameterP
  _   <- char '>'
  return tps

typeParametersT :: [TypeParameter] -> Builder
typeParametersT args = do
  if L.null args
    then mempty
    else singleton '<' <> foldMap typeParameterT args <> singleton '>'

typeParameterP :: Parser TypeParameter
typeParameterP = nameit "TypeParameter" $ do
  id_ <- identifierP
  _   <- char ':'
  cb  <- optional referenceTypeP
  ib  <- many' (char ':' >> referenceTypeP)
  return $ TypeParameter id_ cb ib

typeParameterT :: TypeParameter -> Builder
typeParameterT (TypeParameter n cb ibs) =
  Text.fromText n
    <> singleton ':'
    <> maybe mempty referenceTypeT cb
    <> foldMap (\i -> singleton ':' <> referenceTypeT i) ibs

nameit :: String -> Parser a -> Parser a
nameit str m = m <?> str

identifierP :: Parser Text.Text
identifierP = takeWhile1 (notInClass ".;[/<>:") <?> "Identifier"


methodSignatureP :: Parser MethodSignature
methodSignatureP = do
  tps   <- option [] typeParametersP
  _     <- char '('
  targ  <- many' typeSignatureP
  _     <- char ')'
  res   <- choice [Just <$> typeSignatureP, char 'V' $> Nothing]
  thrws <- many' throwsSignatureP
  return $ MethodSignature tps targ res thrws

methodSignatureToText :: MethodSignature -> Text.Text
methodSignatureToText = LText.toStrict . toLazyText . methodSignatureT

methodSignatureFromText :: Text.Text -> Either String MethodSignature
methodSignatureFromText = parseOnly methodSignatureP

fieldSignatureFromText :: Text.Text -> Either String FieldSignature
fieldSignatureFromText = parseOnly fieldSignatureP

methodSignatureT :: MethodSignature -> Builder
methodSignatureT (MethodSignature tp args res thrws) = do
  typeParametersT tp
    <> singleton '('
    <> foldMap typeSignatureT args
    <> singleton ')'
    <> (case res of
         Nothing -> singleton 'V'
         Just r  -> typeSignatureT r
       )
    <> foldMap throwsSignatureT thrws


throwsSignatureP :: Parser ThrowsSignature
throwsSignatureP = do
  _ <- char '^'
  choice [ThrowsClass <$> classTypeP, ThrowsTypeVariable <$> typeVariableP]


throwsSignatureT :: ThrowsSignature -> Builder
throwsSignatureT t = singleton '^' <> case t of
  ThrowsClass        ct -> classTypeT ct
  ThrowsTypeVariable tt -> typeVariableT tt

fieldSignatureP :: Parser FieldSignature
fieldSignatureP = FieldSignature <$> referenceTypeP

fieldSignatureToText :: FieldSignature -> Text.Text
fieldSignatureToText = LText.toStrict . toLazyText . referenceTypeT . fsRefType

fieldSignatureT :: FieldSignature -> Builder
fieldSignatureT = referenceTypeT . fsRefType

instance Staged Signature where
  evolve (Signature a) = label "Signature" $ Signature <$> link a

  devolve (Signature a) = label "Signature" $ Signature <$> unlink a

$(deriveBaseWithBinary ''Signature)
