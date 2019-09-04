{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Language.JVM.Attribute.RuntimeVisibleAnnotations
Copyright   : (c) Christian Gram Kalhauge, 2018-2019
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Based on the Annotations Attribute, as documented
[here](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.16).
-}
module Language.JVM.Attribute.Annotations
  ( RuntimeVisibleAnnotations (..)
  , RuntimeInvisibleAnnotations (..)
  , RuntimeVisibleParameterAnnotations (..)
  , RuntimeInvisibleParameterAnnotations (..)

  , Annotation (..)
  , ElementValue (..)
  , EnumValue (..)
  , ValuePair (..)
  ) where

-- base
import Data.Char

-- text
import qualified Data.Text as Text

-- binary
import Data.Binary
import qualified Data.Binary.Get as Get

-- jvm-binary
import           Language.JVM.Attribute.Base
import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.Type
import           Language.JVM.Utils

-- | 'RuntimeVisibleAnnotations'
newtype RuntimeVisibleAnnotations r = RuntimeVisibleAnnotations
  { asListOfRuntimeVisibleAnnotations
    :: SizedList16 (Annotation r)
  }

instance IsAttribute (RuntimeVisibleAnnotations Low) where
  attrName = Const "RuntimeVisibleAnnotations"

instance Staged RuntimeVisibleAnnotations where
  stage f (RuntimeVisibleAnnotations m) =
    label "RuntimeVisibleAnnotations"
    $ RuntimeVisibleAnnotations <$> mapM f m

newtype RuntimeInvisibleAnnotations r = RuntimeInvisibleAnnotations
  { asListOfRuntimeInvisibleAnnotations :: SizedList16 (Annotation r)
  }

instance IsAttribute (RuntimeInvisibleAnnotations Low) where
  attrName = Const "RuntimeInvisibleAnnotations"

instance Staged RuntimeInvisibleAnnotations where
  stage f (RuntimeInvisibleAnnotations m) =
    label "RuntimeInvisibleAnnotations"
    $ RuntimeInvisibleAnnotations <$> mapM f m

newtype RuntimeVisibleParameterAnnotations r =
  RuntimeVisibleParameterAnnotations
  { asListOfVisibleParameterAnnotations
    :: SizedList8 (SizedList16 (Annotation r))
  }

-- | 'RuntimeVisibleParameterAnnotations' is an Attribute.
instance IsAttribute (RuntimeVisibleParameterAnnotations Low) where
  attrName = Const "RuntimeVisibleParameterAnnotations"

instance Staged RuntimeVisibleParameterAnnotations where
  stage f (RuntimeVisibleParameterAnnotations m) =
    label "RuntimeVisibleParameterAnnotations"
    $ RuntimeVisibleParameterAnnotations <$> mapM (mapM f) m

newtype RuntimeInvisibleParameterAnnotations r =
  RuntimeInvisibleParameterAnnotations
  { asListOfInvisibleParameterAnnotations
    :: SizedList8 (SizedList16 (Annotation r))
  }

-- | 'RuntimeInvisibleParameterAnnotations' is an Attribute.
instance IsAttribute (RuntimeInvisibleParameterAnnotations Low) where
  attrName = Const "RuntimeInvisibleParameterAnnotations"

instance Staged RuntimeInvisibleParameterAnnotations where
  stage f (RuntimeInvisibleParameterAnnotations m) =
    label "RuntimeInvisibleParameterAnnotations"
    $ RuntimeInvisibleParameterAnnotations <$> mapM (mapM f) m

data Annotation r = Annotation
  { annotationType :: !(Ref Text.Text r)
  , annotationValuePairs :: !(SizedList16 (ValuePair r))
  }

instance Staged Annotation where
   evolve (Annotation t b) = Annotation <$> link t <*> mapM evolve b
   devolve (Annotation t b) = Annotation <$> unlink t <*> mapM devolve b

data ValuePair r = ValuePair
  { name :: !(Ref Text.Text r)
  , value :: !(ElementValue r)
  }

instance Staged ValuePair where
  evolve  (ValuePair t b) = ValuePair <$> link t <*> evolve b
  devolve (ValuePair t b) = ValuePair <$> unlink t <*> devolve b

data ElementValue r
  = EByte           !(Ref VInteger r)
  | EChar           !(Ref VInteger r)
  | EDouble         !(Ref VDouble  r)
  | EFloat          !(Ref VFloat   r)
  | EInt            !(Ref VInteger r)
  | ELong           !(Ref VLong    r)
  | EShort          !(Ref VInteger r)
  | EBoolean        !(Ref VInteger r)
  | EString         !(Ref VString  r)

  | EEnum           !(EnumValue r)
  | EClass          !(Ref ReturnDescriptor r)
  | EAnnotationType !(Annotation r)

  | EArrayType      !(SizedList16 (ElementValue r))

instance Staged ElementValue where
  evolve = \case
    EByte s -> EByte <$> link s
    EChar s -> EChar <$> link s
    EDouble s -> EDouble <$> link s
    EFloat s -> EFloat <$> link s
    EInt s -> EInt <$> link s
    ELong s -> ELong <$> link s
    EShort s -> EShort <$> link s
    EBoolean s -> EBoolean <$> link s
    EString s -> EString <$> link s
    EEnum s -> EEnum <$> evolve s
    EClass s -> EClass <$> link s
    EAnnotationType s -> EAnnotationType <$> evolve s
    EArrayType s -> EArrayType <$> mapM evolve s

  devolve = \case
    EByte s -> EByte <$> unlink s
    EChar s -> EChar <$> unlink s
    EDouble s -> EDouble <$> unlink s
    EFloat s -> EFloat <$> unlink s
    EInt s -> EInt <$> unlink s
    ELong s -> ELong <$> unlink s
    EShort s -> EShort <$> unlink s
    EEnum s -> EEnum <$> devolve s
    EBoolean s -> EBoolean <$> unlink s
    EString s -> EString <$> unlink s
    EClass s -> EClass <$> unlink s
    EAnnotationType s -> EAnnotationType <$> devolve s
    EArrayType s -> EArrayType <$> mapM devolve s

instance Binary (ElementValue Low) where
  get = Get.label "ElementValue" $ getChar8 >>= \case
    'B' -> EByte <$> get
    'C' -> EChar <$> get
    'D' -> EDouble <$> get
    'F' -> EFloat <$> get
    'I' -> EInt <$> get
    'J' -> ELong <$> get
    'S' -> EShort <$> get
    'Z' -> EBoolean <$> get
    's' -> EString <$> get
    'e' -> EEnum <$> get
    'c' -> EClass <$> get
    '@' -> EAnnotationType <$> get
    '[' -> EArrayType <$> get
    c -> fail $ "Does not know " ++ show c

    where getChar8 = chr . fromIntegral <$> Get.getWord8

  put = \case
    EByte a -> putChar8 'B' >> put a
    EChar a -> putChar8 'C' >> put a
    EDouble a -> putChar8 'D' >> put a
    EFloat a -> putChar8 'F' >> put a
    EInt a -> putChar8 'I' >> put a
    ELong a -> putChar8 'J' >> put a
    EShort a -> putChar8 'S' >> put a
    EBoolean a -> putChar8 'Z' >> put a
    EString a -> putChar8 's' >> put a
    EEnum a -> putChar8 'e' >> put a
    EClass a -> putChar8 'c' >> put a
    EAnnotationType a -> putChar8 '@' >> put a
    EArrayType a -> putChar8 '[' >> put a

    where putChar8 = putWord8 . fromIntegral . ord

data EnumValue r = EnumValue
  { enumTypeName ::  !(Ref FieldDescriptor r)
  , enunConstName :: !(Ref Text.Text r)
  }

instance Staged EnumValue where
  evolve (EnumValue n c) = EnumValue <$> link n <*> link c
  devolve (EnumValue n c) = EnumValue <$> unlink n <*> unlink c


-- Type Annoations

-- instance IsAttribute (RuntimeVisibleTypeAnnotations Low) where
--   attrName = Const "RuntimeVisibleTypeAnnotations"

-- newtype RuntimeVisibleTypeAnnotations r =
--   RuntimeVisibleTypeAnnotations
--   { asListOfVisibleTypeAnnotations
--     :: SizedList16 (TypeAnnotation r)
--   }

-- -- | 'RuntimeInvisibleTypeAnnotations' is an Attribute.
-- instance IsAttribute (RuntimeInvisibleTypeAnnotations Low) where
--   attrName = Const "RuntimeInvisibleTypeAnnotations"

-- newtype RuntimeInvisibleTypeAnnotations r =
--   RuntimeInvisibleTypeAnnotations
--   { asListOfInvisibleTypeAnnotations
--     :: SizedList16 (TypeAnnotation r)
--   }

-- data TypeAnnotation r = TypeAnnotation
--   { typeAnnotationTargetInfo :: !TargetInfo
--   , typeAnnotationTargetPath :: !TypePath
--   , typeAnnotationValuePairs :: !(SizedList16 (ValuePair r))
--   }

-- data TargetInfo
--   = TypeParameterTarget !TypeParameterTarget
--   | SuperType !SuperTypeIndex
--   | TypeParameterBoundTarget !TypeParameterBoundTarget
--   | EmptyTarget
--   | FormalParameterTarget !FormalParameterTarget
--   | ThrowsTarget !ThrowsTarget

-- data TypeParameterBoundTarget = TypeParameterBoundTargetX
--   { typeParameterIndex :: TypeParameterTarget
--   , typeBoundIndex     :: Word
--   }

-- type TypeParameterTarget = Word

-- -- | 65535 means supertype, others means interface
-- type SuperTypeIndex = Word16

-- -- | Which Formal Parameter
-- type FormalParameterTarget = Word

-- type ThrowsTarget = Word16

-- type LocalVarTarget = SizedList16 LocalVarEntry

-- data LocalVarEntry = LocalVarEntry
--   { localVarEntryStartPC :: Word16
--   , localVarEntryLength  :: Word16
--   , localVarEntryIndex   :: Word16
--   }

-- type CatchTarget = Word16

-- type OffsetTarget = Word16

-- data TypeArgumentTarget = TypeArgumentTarget
--   { typeArgumentOffset :: Word16
--   , typeArgumentIndex :: Word
--   }

-- type TypePath = SizedList8 TypePathItem

-- data TypePathItem = TypePathItem
--   { typePathKind :: !TypePathKind
--   , typePathIndex :: !Word
--   }

-- data TypePathKind
--   = TPathInArray
--   | TPathInNested
--   | TPathWildcard
--   | TPathTypeArgument

$(deriveBaseWithBinary ''RuntimeInvisibleAnnotations)
$(deriveBaseWithBinary ''RuntimeVisibleAnnotations)
$(deriveBaseWithBinary ''RuntimeInvisibleParameterAnnotations)
$(deriveBaseWithBinary ''RuntimeVisibleParameterAnnotations)
$(deriveBaseWithBinary ''Annotation)
$(deriveBaseWithBinary ''ValuePair)
$(deriveBase ''ElementValue)
$(deriveBaseWithBinary ''EnumValue)
