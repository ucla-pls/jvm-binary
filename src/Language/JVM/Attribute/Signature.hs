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
  ) where

-- import           Control.DeepSeq             (NFData)
-- import           Data.Binary
-- import qualified Data.IntMap                 as IM
import qualified Data.Text                   as Text
-- import           GHC.Generics                (Generic)
import           Language.JVM.Attribute.Base
import           Language.JVM.Staged
import           Language.JVM.Type

instance IsAttribute Signature where
  attrName = Const "Signature"

data Signature a =
  Signature (Ref Text.Text a)

signatureToText :: Signature High -> Text.Text
signatureToText (Signature (RefV s)) = s

signatureFromText :: Text.Text -> Signature High
signatureFromText s = (Signature (RefV s))

data ClassSignature = ClassSignature
  { csTypeParameters :: [TypeParameter]
  , csSuperclassSignature :: ClassTypeSignature
  , csInterfaceSignatures :: [ClassTypeSignature]
  }

data TypeParameter =
  TypeParameter
  { tpIndentifier :: Text.Text
  , tpClassBound :: Maybe ClassName
  , tpInterfaceBound :: [ClassName]
  }

data ClassTypeSignature
  = ClassTypeSignature
    { ctsClassName :: ClassName
    , ctsTypeArguments :: [TypeArgument]
    }
  | InnerClassTypeSignature
    { ctsInnerClassName :: Text.Text
    , ctsTypeArguments :: [TypeArgument]
    }

data TypeArgument
  = AnyTypeArgument
  | TypeArgument
    { taWildcard :: Maybe Wildcard
    , taType :: ReferenceType
    }

data ReferenceType
  = RTClassType ClassTypeSignature
  | RTTypeVariable TypeVariableSignature
  | RTArrayType JavaTypeSignature

newtype TypeVariableSignature
  = TypeVariableSignature Text.Text

data JavaTypeSignature
  = ReferenceType ReferenceType
  | BaseType JBaseType

data Wildcard =
  WPlus | WMinus


instance Staged Signature where
  stage f (Signature a) =
    label "Signature" $ Signature <$> f a

$(deriveBaseWithBinary ''Signature)
