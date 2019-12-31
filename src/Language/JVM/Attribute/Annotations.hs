{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RecordWildCards    #-}
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
  ( RuntimeVisibleAnnotations(..)
  , RuntimeInvisibleAnnotations(..)
  , RuntimeVisibleParameterAnnotations(..)
  , RuntimeInvisibleParameterAnnotations(..)
  , Annotation(..)
  , ElementValue(..)
  , EnumValue(..)
  , ValuePair(..)

  -- * TypeAnnotations
  , TypeAnnotation(..)
  , TypePath
  , TypePathItem(..)
  , TypePathKind(..)
  , RuntimeVisibleTypeAnnotations(..)
  , RuntimeInvisibleTypeAnnotations(..)
  , ClassTypeAnnotation(..)
  , MethodTypeAnnotation(..)
  , FieldTypeAnnotation(..)
  , CodeTypeAnnotation(..)
  , TypeParameterTarget
  , SupertypeTarget
  , isInExtendsClause
  , TypeParameterBoundTarget
  , FormalParameterTarget
  , ThrowsTarget
  , LocalvarTarget
  , LocalvarEntry(..)
  , CatchTarget
  , OffsetTarget
  , TypeArgumentTarget(..)

  -- * AnnotationDefault
  , AnnotationDefault(..)
  )
where

-- base
import           Data.Char
import           Numeric
import           GHC.Generics
import           Unsafe.Coerce

-- text
import qualified Data.Text                     as Text

-- nfdata
import           Control.DeepSeq

-- binary
import           Data.Binary
import qualified Data.Binary.Get               as Get

-- jvm-binary
import           Language.JVM.Attribute.Base
import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.ByteCode
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
    label "RuntimeVisibleAnnotations" $ RuntimeVisibleAnnotations <$> mapM f m

newtype RuntimeInvisibleAnnotations r = RuntimeInvisibleAnnotations
  { asListOfRuntimeInvisibleAnnotations :: SizedList16 (Annotation r)
  }

instance IsAttribute (RuntimeInvisibleAnnotations Low) where
  attrName = Const "RuntimeInvisibleAnnotations"

instance Staged RuntimeInvisibleAnnotations where
  stage f (RuntimeInvisibleAnnotations m) =
    label "RuntimeInvisibleAnnotations"
      $   RuntimeInvisibleAnnotations
      <$> mapM f m

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
      $   RuntimeVisibleParameterAnnotations
      <$> mapM (mapM f) m

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
      $   RuntimeInvisibleParameterAnnotations
      <$> mapM (mapM f) m

data Annotation r = Annotation
  { annotationType :: !(Ref FieldDescriptor r)
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
  evolve (ValuePair t b) = ValuePair <$> link t <*> evolve b
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
    EByte           s -> EByte <$> link s
    EChar           s -> EChar <$> link s
    EDouble         s -> EDouble <$> link s
    EFloat          s -> EFloat <$> link s
    EInt            s -> EInt <$> link s
    ELong           s -> ELong <$> link s
    EShort          s -> EShort <$> link s
    EBoolean        s -> EBoolean <$> link s
    EString         s -> EString <$> link s
    EEnum           s -> EEnum <$> evolve s
    EClass          s -> EClass <$> link s
    EAnnotationType s -> EAnnotationType <$> evolve s
    EArrayType      s -> EArrayType <$> mapM evolve s

  devolve = \case
    EByte           s -> EByte <$> unlink s
    EChar           s -> EChar <$> unlink s
    EDouble         s -> EDouble <$> unlink s
    EFloat          s -> EFloat <$> unlink s
    EInt            s -> EInt <$> unlink s
    ELong           s -> ELong <$> unlink s
    EShort          s -> EShort <$> unlink s
    EEnum           s -> EEnum <$> devolve s
    EBoolean        s -> EBoolean <$> unlink s
    EString         s -> EString <$> unlink s
    EClass          s -> EClass <$> unlink s
    EAnnotationType s -> EAnnotationType <$> devolve s
    EArrayType      s -> EArrayType <$> mapM devolve s

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
    c   -> fail $ "Does not know " ++ show c
    where getChar8 = chr . fromIntegral <$> Get.getWord8

  put = \case
    EByte           a -> putChar8 'B' >> put a
    EChar           a -> putChar8 'C' >> put a
    EDouble         a -> putChar8 'D' >> put a
    EFloat          a -> putChar8 'F' >> put a
    EInt            a -> putChar8 'I' >> put a
    ELong           a -> putChar8 'J' >> put a
    EShort          a -> putChar8 'S' >> put a
    EBoolean        a -> putChar8 'Z' >> put a
    EString         a -> putChar8 's' >> put a
    EEnum           a -> putChar8 'e' >> put a
    EClass          a -> putChar8 'c' >> put a
    EAnnotationType a -> putChar8 '@' >> put a
    EArrayType      a -> putChar8 '[' >> put a
    where putChar8 = putWord8 . fromIntegral . ord

data EnumValue r = EnumValue
  { enumTypeName ::  !(Ref FieldDescriptor r)
  , enunConstName :: !(Ref Text.Text r)
  }

instance Staged EnumValue where
  evolve (EnumValue n c) = EnumValue <$> link n <*> link c
  devolve (EnumValue n c) = EnumValue <$> unlink n <*> unlink c

-- Type Annoations

-- | A 'TypeAnnotation' is targeting different types.
data TypeAnnotation m r = TypeAnnotation
  { typeAnnotationTarget     :: !(m r)
  , typeAnnotationPath       :: !TypePath
  , typeAnnotationType       :: !(Ref FieldDescriptor r)
  , typeAnnotationValuePairs :: SizedList16 (ValuePair r)
  }

instance Binary (m Low) => Binary (TypeAnnotation m Low) where
  get = TypeAnnotation <$> get <*> get <*> get <*> get
  put TypeAnnotation {..} =
    put typeAnnotationTarget
      >> put typeAnnotationPath
      >> put typeAnnotationType
      >> put typeAnnotationValuePairs

instance Staged m => Staged (TypeAnnotation m) where
  evolve TypeAnnotation {..} =
    TypeAnnotation
      <$> evolve typeAnnotationTarget
      <*> pure typeAnnotationPath
      <*> link typeAnnotationType
      <*> mapM evolve typeAnnotationValuePairs
  devolve TypeAnnotation {..} =
    TypeAnnotation
      <$> devolve typeAnnotationTarget
      <*> pure typeAnnotationPath
      <*> unlink typeAnnotationType
      <*> mapM devolve typeAnnotationValuePairs

instance ByteCodeStaged m => ByteCodeStaged (TypeAnnotation m) where
  evolveBC f TypeAnnotation {..} =
    TypeAnnotation
      <$> evolveBC f typeAnnotationTarget
      <*> pure typeAnnotationPath
      <*> link typeAnnotationType
      <*> mapM evolve typeAnnotationValuePairs

  devolveBC f TypeAnnotation {..} =
    TypeAnnotation
      <$> devolveBC f typeAnnotationTarget
      <*> pure typeAnnotationPath
      <*> unlink typeAnnotationType
      <*> mapM devolve typeAnnotationValuePairs

deriving instance Show (m High) => Show (TypeAnnotation m High)
deriving instance Eq (m High) => Eq (TypeAnnotation m High)
deriving instance Generic (m High) => Generic (TypeAnnotation m High)
deriving instance (Generic (m High), NFData (m High)) => NFData (TypeAnnotation m High)

deriving instance Show (m Low) => Show (TypeAnnotation m Low)
deriving instance Eq (m Low) => Eq (TypeAnnotation m Low)
deriving instance Ord (m Low) => Ord (TypeAnnotation m Low)
deriving instance Generic (m Low) => Generic (TypeAnnotation m Low)
deriving instance (Generic (m Low), NFData (m Low)) => NFData (TypeAnnotation m Low)

type TypePath = SizedList8 TypePathItem

data TypePathItem = TypePathItem
  { typePathKind :: !TypePathKind
  , typePathIndex :: !Word8
  } deriving (Show, Eq, Ord, Generic, NFData)

data TypePathKind
  = TPathInArray
  | TPathInNested
  | TPathWildcard
  | TPathTypeArgument
  deriving (Show, Eq, Ord, Generic, NFData)

instance Binary TypePathItem where
  get = do
    typePathKind <- getWord8 >>= \case
      0 -> pure TPathInArray
      1 -> pure TPathInNested
      2 -> pure TPathWildcard
      3 -> pure TPathTypeArgument
      a -> fail $ "Expected unsigned byte in range [0:3] got: " ++ show a
    typePathIndex <- getWord8
    pure $ TypePathItem { .. }

  put TypePathItem {..} = do
    putWord8 $ case typePathKind of
      TPathInArray      -> 0
      TPathInNested     -> 1
      TPathWildcard     -> 2
      TPathTypeArgument -> 3
    putWord8 typePathIndex


-- | From [here](https://docs.oracle.com/javase/specs/jvms/se9/html/jvms-4.html#jvms-4.7.20-400)
data ClassTypeAnnotation r
  = ClassTypeParameterDeclaration !TypeParameterTarget
    -- ^ type parameter declaration of generic class or interface (0x00)
  | ClassSuperType !SupertypeTarget
    -- ^ type in extends clause of class or interface declaration, or in
    -- implements clause of interface declaration (0x10)
  | ClassBoundTypeParameterDeclaration !TypeParameterBoundTarget
    -- ^ type in bound of type parameter declaration of generic class or
    -- interface (0x11)

instance Binary (ClassTypeAnnotation Low) where
  get = getWord8 >>= \case
    0x00 -> ClassTypeParameterDeclaration <$> get
    0x10 -> ClassSuperType <$> get
    0x11 -> ClassBoundTypeParameterDeclaration <$> get
    a ->
      fail
        $  "Unexpected target type "
        ++ showHex a ""
        ++ " in class type annotation"

  put = \case
    ClassTypeParameterDeclaration      a -> putWord8 0x00 >> put a
    ClassSuperType                     a -> putWord8 0x10 >> put a
    ClassBoundTypeParameterDeclaration a -> putWord8 0x11 >> put a

instance Staged ClassTypeAnnotation where
  stage _ = unsafeCoerce

data MethodTypeAnnotation r
  = MethodTypeParameterDeclaration !TypeParameterTarget
  -- ^ type parameter declaration of generic method or constructor (0x01)
  | MethodBoundTypeParameterDeclaration !TypeParameterBoundTarget
  -- ^ type in bound of type parameter declaration of generic method or
  -- constructor (0x12)
  | MethodReturnType
  -- ^ return type of method or constructor (0x14)
  | MethodReceiverType
  -- ^ receiver type of method or constructor (0x15)
  | MethodFormalParameter !FormalParameterTarget
  -- ^ type in formal parameter declaration of method, constructor, or lambda
  -- expression (0x16)
  | MethodThrowsClause !ThrowsTarget
  -- ^ type in throws clause of method or constructor (0x17)

instance Binary (MethodTypeAnnotation Low) where
  get = getWord8 >>= \case
    0x01 -> MethodTypeParameterDeclaration <$> get
    0x12 -> MethodBoundTypeParameterDeclaration <$> get
    0x14 -> pure MethodReturnType
    0x15 -> pure MethodReceiverType
    0x16 -> MethodFormalParameter <$> get
    0x17 -> MethodThrowsClause <$> get
    a ->
      fail
        $  "Unexpected target type "
        ++ showHex a ""
        ++ " in method type annotation"

  put = \case
    MethodTypeParameterDeclaration      a -> putWord8 0x01 >> put a
    MethodBoundTypeParameterDeclaration a -> putWord8 0x12 >> put a
    MethodReturnType                      -> putWord8 0x14
    MethodReceiverType                    -> putWord8 0x15
    MethodFormalParameter a               -> putWord8 0x16 >> put a
    MethodThrowsClause    a               -> putWord8 0x17 >> put a

instance Staged MethodTypeAnnotation where
  stage _ = unsafeCoerce

data FieldTypeAnnotation r =
  FieldTypeAnnotation
  -- ^ type in field declaration (0x13)

instance Staged FieldTypeAnnotation where
  stage _ = unsafeCoerce

instance Binary (FieldTypeAnnotation Low) where
  get = getWord8 >>= \case
    0x13 -> pure FieldTypeAnnotation
    a ->
      fail
        $  "Unexpected target type "
        ++ showHex a ""
        ++ " in field type annotation"

  put _ = putWord8 0x13

data CodeTypeAnnotation r
  = LocalVariableDeclaration !(LocalvarTarget r)
  -- ^ type in local variable declaration (0x40)
  | ResourceVariableDeclaration !(LocalvarTarget r)
  -- ^  type in resource variable declaration  (0x41)
  | ExceptionParameterDeclaration !CatchTarget
  -- ^ type in exception parameter declaration (0x42)
  | InstanceOfExpression !(OffsetTarget r)
  -- ^ type in instanceof expression (0x43)
  | NewExpression !(OffsetTarget r)
  -- ^ type in new expression (0x44)
  | NewMethodReferenceExpression !(OffsetTarget r)
  -- ^ type in method reference expression using ::new (0x45)
  | IdentifierMethodReferenceExpression !(OffsetTarget r)
  -- ^ type in method reference expression using ::Identifier (0x46)
  | CastExpression !(TypeArgumentTarget r)
  -- ^ type in cast expression (0x47)
  | ConstructorExpression !(TypeArgumentTarget r)
  -- ^ type argument for generic constructor in new expression or explicit
  -- constructor invocation statement (0x48)
  | MethodIncovationExpression !(TypeArgumentTarget r)
  -- ^ type argument for generic method in method invocation expression (0x49)
  | GenericNewMethodReferenceExpression !(TypeArgumentTarget r)
  -- ^ type argument for generic constructor in method reference expression using ::new (0x4A)
  | GenericIdentifierwMethodReferenceExpression !(TypeArgumentTarget r)
  -- ^ type argument for generic method in method reference expression using ::Identifier (0x4B)

instance Binary (CodeTypeAnnotation Low) where
  get = getWord8 >>= \case
    0x40 -> LocalVariableDeclaration <$> get
    0x41 -> ResourceVariableDeclaration <$> get
    0x42 -> ExceptionParameterDeclaration <$> get
    0x43 -> InstanceOfExpression <$> get
    0x44 -> NewExpression <$> get
    0x45 -> NewMethodReferenceExpression <$> get
    0x46 -> IdentifierMethodReferenceExpression <$> get
    0x47 -> CastExpression <$> get
    0x48 -> ConstructorExpression <$> get
    0x49 -> MethodIncovationExpression <$> get
    0x4A -> GenericNewMethodReferenceExpression <$> get
    0x4B -> GenericIdentifierwMethodReferenceExpression <$> get
    a ->
      fail
        $  "Unexpected target type "
        ++ showHex a ""
        ++ " in code type annotation"

  put = \case
    LocalVariableDeclaration                    a -> putWord8 0x40 >> put a
    ResourceVariableDeclaration                 a -> putWord8 0x41 >> put a
    ExceptionParameterDeclaration               a -> putWord8 0x42 >> put a
    InstanceOfExpression                        a -> putWord8 0x43 >> put a
    NewExpression                               a -> putWord8 0x44 >> put a
    NewMethodReferenceExpression                a -> putWord8 0x45 >> put a
    IdentifierMethodReferenceExpression         a -> putWord8 0x46 >> put a
    CastExpression                              a -> putWord8 0x47 >> put a
    ConstructorExpression                       a -> putWord8 0x48 >> put a
    MethodIncovationExpression                  a -> putWord8 0x49 >> put a
    GenericNewMethodReferenceExpression         a -> putWord8 0x4A >> put a
    GenericIdentifierwMethodReferenceExpression a -> putWord8 0x4B >> put a

instance ByteCodeStaged CodeTypeAnnotation where
  evolveBC ev = \case
    LocalVariableDeclaration a ->
      LocalVariableDeclaration <$> mapM (evolveBC ev) a
    ResourceVariableDeclaration a ->
      ResourceVariableDeclaration <$> mapM (evolveBC ev) a
    ExceptionParameterDeclaration a -> pure $ ExceptionParameterDeclaration a
    InstanceOfExpression          a -> InstanceOfExpression <$> ev a
    NewExpression                 a -> NewExpression <$> ev a
    NewMethodReferenceExpression  a -> NewMethodReferenceExpression <$> ev a
    IdentifierMethodReferenceExpression a ->
      IdentifierMethodReferenceExpression <$> ev a
    CastExpression        a -> CastExpression <$> evolveBC ev a
    ConstructorExpression a -> ConstructorExpression <$> evolveBC ev a
    MethodIncovationExpression a ->
      MethodIncovationExpression <$> evolveBC ev a
    GenericNewMethodReferenceExpression a ->
      GenericNewMethodReferenceExpression <$> evolveBC ev a
    GenericIdentifierwMethodReferenceExpression a ->
      GenericIdentifierwMethodReferenceExpression <$> evolveBC ev a

  devolveBC dev = \case
    LocalVariableDeclaration a ->
      LocalVariableDeclaration <$> mapM (devolveBC dev) a
    ResourceVariableDeclaration a ->
      ResourceVariableDeclaration <$> mapM (devolveBC dev) a
    ExceptionParameterDeclaration a -> pure $ ExceptionParameterDeclaration a
    InstanceOfExpression          a -> InstanceOfExpression <$> dev a
    NewExpression                 a -> NewExpression <$> dev a
    NewMethodReferenceExpression  a -> NewMethodReferenceExpression <$> dev a
    IdentifierMethodReferenceExpression a ->
      IdentifierMethodReferenceExpression <$> dev a
    CastExpression        a -> CastExpression <$> devolveBC dev a
    ConstructorExpression a -> ConstructorExpression <$> devolveBC dev a
    MethodIncovationExpression a ->
      MethodIncovationExpression <$> devolveBC dev a
    GenericNewMethodReferenceExpression a ->
      GenericNewMethodReferenceExpression <$> devolveBC dev a
    GenericIdentifierwMethodReferenceExpression a ->
      GenericIdentifierwMethodReferenceExpression <$> devolveBC dev a

-- | The 'TypeParameterTarget' item indicates that an annotation appears on the
-- declaration of the i'th type parameter of a generic class, generic interface,
-- generic method, or generic constructor.
type TypeParameterTarget = Word8

-- | The 'SupertypeTarget' item indicates that an annotation appears on a type in
-- the extends or implements clause of a class or interface declaration.
--
-- A value of 65535 specifies that the annotation appears on the superclass in
-- an extends clause of a class declaration.
type SupertypeTarget = Word16

-- | Check if the 'SupertypeTarget' is in the extends clauses
isInExtendsClause :: SupertypeTarget -> Bool
isInExtendsClause st = st == 0xFFFF

-- | The 'TypeParameterBoundTarget' item indicates that an annotation appears
-- on the i'th bound of the j'th type parameter declaration of a generic class,
-- interface, method, or constructor.
data TypeParameterBoundTarget = TypeParameterBoundTarget
  { typeParameter :: !TypeParameterTarget
  , typeBound :: !Word8
  } deriving (Eq, Show, Ord, Generic, NFData)

instance Binary TypeParameterBoundTarget

-- | The 'FormalParameterTarget' item indicates that an annotation appears on
-- the type in a formal parameter declaration of a method, constructor, or
-- lambda expression. The target is 0-indexed.
type FormalParameterTarget = Word8

-- | The 'ThrowsTarget' item indicates that an annotation appears on the i'th
-- type in the throws clause of a method or constructor declaration.
--
-- The value is an index into the Exceptions attribute
type ThrowsTarget = Word16

-- | The 'LocalvarTarget' item indicates that an annotation appears on the type
-- in a local variable declaration, including a variable declared as a resource
-- in a try-with-resources statement.
--
-- The table is needed because a variable might span multiple live ranges.
type LocalvarTarget r = SizedList16 (LocalvarEntry r)

-- | An entry in the Localvar Table
data LocalvarEntry r = LocalvarEntry
  { lvStartPc :: !(ByteCodeRef r)
  , lvLength :: !Word16
  , lvLocalVarIndex :: !Word16
  }

instance ByteCodeStaged LocalvarEntry where
  evolveBC ev LocalvarEntry {..} =
    LocalvarEntry <$> ev lvStartPc <*> pure lvLength <*> pure lvLocalVarIndex

  devolveBC dev LocalvarEntry {..} =
    LocalvarEntry <$> dev lvStartPc <*> pure lvLength <*> pure lvLocalVarIndex

-- | The 'CatchTarget' item indicates that an annotation appears on the i'th
-- type in an exception parameter declaration.
type CatchTarget = Word16

-- | The 'OffsetTarget' item indicates that an annotation appears on either the
-- type in an instanceof expression or a new expression, or the type before the
-- :: in a method reference expression.
type OffsetTarget r = ByteCodeRef r

-- | The 'TypeArgumentTarget' item indicates that an annotation appears either
-- on the i'th type in a cast expression, or on the i'th type argument in the
-- explicit type argument list for any of the following: a new expression, an
-- explicit constructor invocation statement, a method invocation expression, or a
-- method reference expression.
data TypeArgumentTarget r = TypeArgumentTarget
  { typeArgumentOffset :: !(ByteCodeRef r)
  , typeArgumentIndex  :: Word8
  }

instance ByteCodeStaged TypeArgumentTarget where
  evolveBC ev TypeArgumentTarget {..} =
    TypeArgumentTarget <$> ev typeArgumentOffset <*> pure typeArgumentIndex

  devolveBC dev TypeArgumentTarget {..} =
    TypeArgumentTarget <$> dev typeArgumentOffset <*> pure typeArgumentIndex


instance (Generic (m Low), Binary (m Low)) => IsAttribute (RuntimeVisibleTypeAnnotations m Low) where
  attrName = Const "RuntimeVisibleTypeAnnotations"

instance (Staged m) => Staged (RuntimeVisibleTypeAnnotations m) where
  stage f (RuntimeVisibleTypeAnnotations a) =
    RuntimeVisibleTypeAnnotations <$> mapM f a

instance ByteCodeStaged m => ByteCodeStaged (RuntimeVisibleTypeAnnotations m) where
  evolveBC f (RuntimeVisibleTypeAnnotations a) =
    RuntimeVisibleTypeAnnotations <$> mapM (evolveBC f) a

  devolveBC f (RuntimeVisibleTypeAnnotations a) =
    RuntimeVisibleTypeAnnotations <$> mapM (devolveBC f) a

newtype RuntimeVisibleTypeAnnotations m r =
  RuntimeVisibleTypeAnnotations
  { asListOfVisibleTypeAnnotations
    :: SizedList16 (TypeAnnotation m r)
  }

-- | 'RuntimeInvisibleTypeAnnotations' is an Attribute.
instance (Generic (m Low), Binary (m Low)) => IsAttribute (RuntimeInvisibleTypeAnnotations m Low) where
  attrName = Const "RuntimeInvisibleTypeAnnotations"

instance Staged m => Staged (RuntimeInvisibleTypeAnnotations m) where
  stage f (RuntimeInvisibleTypeAnnotations a) =
    RuntimeInvisibleTypeAnnotations <$> mapM f a

instance ByteCodeStaged m => ByteCodeStaged (RuntimeInvisibleTypeAnnotations m) where
  evolveBC f (RuntimeInvisibleTypeAnnotations a) =
    RuntimeInvisibleTypeAnnotations <$> mapM (evolveBC f) a

  devolveBC f (RuntimeInvisibleTypeAnnotations a) =
    RuntimeInvisibleTypeAnnotations <$> mapM (devolveBC f) a

newtype RuntimeInvisibleTypeAnnotations m r =
  RuntimeInvisibleTypeAnnotations
  { asListOfInvisibleTypeAnnotations
    :: SizedList16 (TypeAnnotation m r)
  }

deriving instance Show (m High) => Show (RuntimeVisibleTypeAnnotations m High)
deriving instance Eq (m High) => Eq (RuntimeVisibleTypeAnnotations m High)
deriving instance Generic (m High) => Generic (RuntimeVisibleTypeAnnotations m High)
deriving instance (Generic (m High), NFData (m High)) => NFData (RuntimeVisibleTypeAnnotations m High)

deriving instance Show (m Low) => Show (RuntimeVisibleTypeAnnotations m Low)
deriving instance Eq (m Low) => Eq (RuntimeVisibleTypeAnnotations m Low)
deriving instance Ord (m Low) => Ord (RuntimeVisibleTypeAnnotations m Low)
deriving instance Generic (m Low) => Generic (RuntimeVisibleTypeAnnotations m Low)
deriving instance (Generic (m Low), NFData (m Low)) => NFData (RuntimeVisibleTypeAnnotations m Low)
deriving instance (Generic (m Low), Binary (m Low)) => Binary (RuntimeVisibleTypeAnnotations m Low)

deriving instance Show (m High) => Show (RuntimeInvisibleTypeAnnotations m High)
deriving instance Eq (m High) => Eq (RuntimeInvisibleTypeAnnotations m High)
deriving instance Generic (m High) => Generic (RuntimeInvisibleTypeAnnotations m High)
deriving instance (Generic (m High), NFData (m High)) => NFData (RuntimeInvisibleTypeAnnotations m High)

deriving instance Show (m Low) => Show (RuntimeInvisibleTypeAnnotations m Low)
deriving instance Eq (m Low) => Eq (RuntimeInvisibleTypeAnnotations m Low)
deriving instance Ord (m Low) => Ord (RuntimeInvisibleTypeAnnotations m Low)
deriving instance Generic (m Low) => Generic (RuntimeInvisibleTypeAnnotations m Low)
deriving instance (Generic (m Low), NFData (m Low)) => NFData (RuntimeInvisibleTypeAnnotations m Low)

deriving instance (Generic (m Low), Binary (m Low)) => Binary (RuntimeInvisibleTypeAnnotations m Low)

-- | The AnnotationDefault attribute is a variable-length attribute in the
-- attributes table of certain method_info structures (§4.6), namely those
-- representing elements of annotation types (JLS §9.6.1). The AnnotationDefault
-- attribute records the default value (JLS §9.6.2) for the element represented by
-- the method_info structure. The Java Virtual Machine must make this default value
-- available so it can be applied by appropriate reflective APIs.
newtype AnnotationDefault r = AnnotationDefault
  { defaultValue :: ElementValue r
  }

instance IsAttribute (AnnotationDefault Low) where
  attrName = Const "AnnotationDefault"

instance Staged AnnotationDefault where
  stage f AnnotationDefault {..} = AnnotationDefault <$> stage f defaultValue


$(deriveBaseWithBinary ''RuntimeInvisibleAnnotations)
$(deriveBaseWithBinary ''RuntimeVisibleAnnotations)
$(deriveBaseWithBinary ''RuntimeInvisibleParameterAnnotations)
$(deriveBaseWithBinary ''RuntimeVisibleParameterAnnotations)
$(deriveBaseWithBinary ''Annotation)
$(deriveBaseWithBinary ''ValuePair)
$(deriveBaseWithBinary ''AnnotationDefault)
$(deriveBase ''ElementValue)

$(deriveBase ''ClassTypeAnnotation)
$(deriveBase ''MethodTypeAnnotation)
$(deriveBase ''FieldTypeAnnotation)
$(deriveBase ''CodeTypeAnnotation)

$(deriveBaseWithBinary ''LocalvarEntry)
$(deriveBaseWithBinary ''TypeArgumentTarget)

$(deriveBaseWithBinary ''EnumValue)
