{-# LANGUAGE DeriveAnyClass     #-}
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
  ( Signature (..)
  , signatureToText
  , signatureFromText

  -- * Top Level Definitions

  , ClassSignature (..)
  , classSignatureToText
  , classSignatureFromText
  , classSignatureP
  , MethodSignature (..)
  , methodSignatureToText
  , methodSignatureFromText
  , methodSignatureP
  , FieldSignature (..)
  , fieldSignatureToText
  , fieldSignatureFromText
  , fieldSignatureP

  -- * Lower Level Definitions
  , ClassType (..)
  , classTypeP
  , ReferenceType (..)
  , referenceTypeP
  , ThrowsSignature (..)
  , TypeArgument (..)
  , TypeParameter (..)
  , TypeSignature (..)
  , TypeVariable (..)
  , typeVariableP
  , Wildcard (..)
  , typeParameterP
  , typeParametersP
  ) where

import           Control.DeepSeq             (NFData)
import qualified Data.Text                   as Text

import qualified Data.Text.Lazy              as LText
import           Data.Text.Lazy.Builder      as Text

import           Data.Functor
import           GHC.Generics                (Generic)

import           Data.Attoparsec.Text

import qualified Data.List                   as L

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

----------------------
-- Parsing
----------------------

data ClassSignature = ClassSignature
  { csTypeParameters      :: [TypeParameter]
  , csSuperclassSignature :: ClassType
  , csInterfaceSignatures :: [ClassType]
  }
  deriving (Show, Eq, Generic, NFData)

classSignatureP :: Parser ClassSignature
classSignatureP = do
  tp <- option [] typeParametersP
  ss <- classTypeP
  is <- many' classTypeP
  return $ ClassSignature tp ss is

classSignatureToText :: ClassSignature -> Text.Text
classSignatureToText =
  LText.toStrict . toLazyText . classSignatureT

classSignatureFromText :: Text.Text -> Either String ClassSignature
classSignatureFromText =
  parseOnly classSignatureP

classSignatureT :: ClassSignature -> Builder
classSignatureT (ClassSignature tp ct its)= do
  typeParametersT tp <> foldMap classTypeT (ct:its)

data TypeSignature
  = ReferenceType ReferenceType
  | BaseType JBaseType
  deriving (Show, Eq, Generic, NFData)

typeSignatureP :: Parser TypeSignature
typeSignatureP = do
  choice [ (ReferenceType <$> referenceTypeP) <?> "JRefereceType"
         , (BaseType <$> parseType) <?> "JBaseType" ]

typeSignatureT :: TypeSignature -> Builder
typeSignatureT (ReferenceType t) = referenceTypeT t
typeSignatureT (BaseType t)      = singleton (jBaseTypeToChar t)

data ReferenceType
  = RefClassType ClassType
  | RefTypeVariable TypeVariable
  | RefArrayType TypeSignature
  deriving (Show, Eq, Generic, NFData)

referenceTypeP :: Parser ReferenceType
referenceTypeP = do
  choice
    [ RefClassType <$> classTypeP
    , RefTypeVariable <$> typeVariableP
    , RefArrayType <$> (char '[' >> typeSignatureP)
    ]

referenceTypeT :: ReferenceType -> Builder
referenceTypeT t =
  case t of
    RefClassType ct    -> classTypeT ct
    RefTypeVariable tv -> typeVariableT tv
    RefArrayType at    -> singleton '[' <> typeSignatureT at

data ClassType
  = ClassType
    { ctsClassName     :: ClassName
    , ctsTypeArguments :: [Maybe TypeArgument]
    }
  | InnerClassType
    { ctsInnerClassName :: Text.Text
    , ctsOuterClassType :: ClassType
    , ctsTypeArguments  :: [Maybe TypeArgument]
    }
  deriving (Show, Eq, Generic, NFData)

classTypeP :: Parser ClassType
classTypeP = nameit "ClassType" $ do
  _ <- char 'L'
  cn <- parseType
  ta <- option [] typeArgumentsP
  ict <- many' $ do
      _ <- char '.'
      i <- identifierP
      ta' <- option [] typeArgumentsP
      return (i, ta')
  _ <- char ';'
  return $ L.foldl' (\a (i,ta') -> InnerClassType i a ta') (ClassType cn ta) ict

classTypeT :: ClassType -> Builder
classTypeT t =
  go t <> singleton ';'
  where
    go t' =
      case t' of
        InnerClassType n ct arg ->
          go ct <> singleton '.' <> Text.fromText n <> typeArgumentsT arg
        ClassType cn arg ->
          singleton 'L'
          <> Text.fromText (classNameAsText cn)
          <> typeArgumentsT arg

data TypeArgument = TypeArgument
  { taWildcard :: Maybe Wildcard
  , taType     :: ReferenceType
  }
  deriving (Show, Eq, Generic, NFData)

typeArgumentsP :: Parser [ Maybe TypeArgument ]
typeArgumentsP = do
  _ <- char '<'
  tas <- many1' typeArgumentP
  _ <- char '>'
  return tas

typeArgumentP :: Parser (Maybe TypeArgument)
typeArgumentP = do
  choice [ Just
           <$> ( TypeArgument
                  <$> option Nothing (Just <$> wildcardP)
                  <*> referenceTypeP
               )
         , char '*' $> Nothing
         ] <?> "TypeArgument"

typeArgumentsT :: [ Maybe TypeArgument ] -> Builder
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
        Just WildMinus -> singleton '-'
        Just WildPlus  -> singleton '+'
        Nothing        -> mempty) <> referenceTypeT rt

data Wildcard =
  WildPlus | WildMinus
  deriving (Show, Eq, Generic, NFData)

wildcardP :: Parser Wildcard
wildcardP = choice [ char '+' $> WildPlus, char '-' $> WildMinus]

newtype TypeVariable =
  TypeVariable { tvAsText :: Text.Text }
  deriving (Show, Eq, Generic, NFData)

typeVariableP :: Parser TypeVariable
typeVariableP = do
  _ <- char 'T'
  t <- identifierP
  _ <- char ';'
  return $ TypeVariable t

typeVariableT :: TypeVariable -> Builder
typeVariableT (TypeVariable t)= do
  singleton 'T' <> Text.fromText t <> singleton ';'

data TypeParameter =
  TypeParameter
  { tpIndentifier    :: Text.Text
  , tpClassBound     :: Maybe ReferenceType
  , tpInterfaceBound :: [ReferenceType]
  }
  deriving (Show, Eq, Generic, NFData)

typeParametersP :: Parser [TypeParameter]
typeParametersP = nameit "TypeParameters" $ do
  _ <- char '<'
  tps <- many1' typeParameterP
  _ <- char '>'
  return tps

typeParametersT :: [ TypeParameter ] -> Builder
typeParametersT args = do
  if L.null args
    then mempty
    else singleton '<' <> foldMap typeParameterT args <> singleton '>'

typeParameterP :: Parser TypeParameter
typeParameterP = nameit "TypeParameter" $ do
  id_ <- identifierP
  _ <- char ':'
  cb <- option Nothing (Just <$> referenceTypeP)
  ib <- many' (char ':' >> referenceTypeP)
  return $ TypeParameter id_ cb ib

typeParameterT :: TypeParameter -> Builder
typeParameterT (TypeParameter n cb ibs) =
  Text.fromText  n <> singleton ':' <> maybe mempty referenceTypeT cb <>
    foldMap (\i -> singleton ':' <> referenceTypeT i) ibs

nameit :: String -> Parser a -> Parser a
nameit str m = m <?> str

identifierP :: Parser Text.Text
identifierP =
  takeWhile1 (notInClass ".;[/<>:") <?> "Identifier"


data MethodSignature = MethodSignature
  { msTypeParameters :: [TypeParameter]
  , msArguments      :: [TypeSignature]
  , msResults        :: Maybe TypeSignature
  , msThrows         :: [ ThrowsSignature ]
  }
  deriving (Show, Eq, Generic, NFData)

methodSignatureP :: Parser MethodSignature
methodSignatureP = do
  tps <- option [] typeParametersP
  _ <- char '('
  targ <- many' typeSignatureP
  _ <- char ')'
  res <- choice [ Just <$> typeSignatureP, char 'V' $> Nothing ]
  thrws <- many' throwsSignatureP
  return $ MethodSignature tps targ res thrws

methodSignatureToText :: MethodSignature -> Text.Text
methodSignatureToText =
  LText.toStrict . toLazyText . methodSignatureT

methodSignatureFromText :: Text.Text -> Either String MethodSignature
methodSignatureFromText =
  parseOnly methodSignatureP

fieldSignatureFromText :: Text.Text -> Either String FieldSignature
fieldSignatureFromText =
  parseOnly fieldSignatureP

methodSignatureT :: MethodSignature -> Builder
methodSignatureT (MethodSignature tp args res thrws)= do
  typeParametersT tp
    <> singleton '('
    <> foldMap typeSignatureT args
    <> singleton ')'
    <> (case res of Nothing -> singleton 'V'; Just r -> typeSignatureT r)
    <> foldMap throwsSignatureT thrws

data ThrowsSignature
  = ThrowsClass ClassType
  | ThrowsTypeVariable TypeVariable
  deriving (Show, Eq, Generic, NFData)

throwsSignatureP :: Parser ThrowsSignature
throwsSignatureP = do
  _ <- char '^'
  choice [ ThrowsClass <$> classTypeP, ThrowsTypeVariable <$> typeVariableP]


throwsSignatureT :: ThrowsSignature -> Builder
throwsSignatureT t =
  singleton '^'
    <> case t of
         ThrowsClass ct        -> classTypeT ct
         ThrowsTypeVariable tt -> typeVariableT tt

newtype FieldSignature =
  FieldSignature {fsRefType :: ReferenceType}
  deriving (Show, Eq, Generic, NFData)

fieldSignatureP :: Parser FieldSignature
fieldSignatureP =
  FieldSignature <$> referenceTypeP

fieldSignatureToText :: FieldSignature -> Text.Text
fieldSignatureToText =
  LText.toStrict . toLazyText . referenceTypeT . fsRefType

instance Staged Signature where
  evolve (Signature a) =
    label "Signature" $ Signature <$> link a

  devolve (Signature a) =
    label "Signature" $ Signature <$> unlink a

$(deriveBaseWithBinary ''Signature)
