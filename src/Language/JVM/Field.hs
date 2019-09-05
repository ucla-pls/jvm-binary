{-|
Module      : Language.JVM.Field
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Language.JVM.Field
  ( Field (..)
  , fAccessFlags
  -- * Attributes
  , fConstantValue
  , fSignature
  , FieldAttributes (..)
  , emptyFieldAttributes
  ) where


import Data.Monoid
import qualified Data.Set                as Set
import qualified Data.Text               as Text

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.Utils
import           Language.JVM.Type


-- | A Field in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5).
data Field r = Field
  { fAccessFlags'  :: !(BitSet16 FAccessFlag)
  , fName          :: !(Ref Text.Text r)
  , fDescriptor    :: !(Ref FieldDescriptor r)
  , fAttributes    :: !(Attributes FieldAttributes r)
  }

-- | Get the set of access flags
fAccessFlags :: Field r -> Set.Set FAccessFlag
fAccessFlags = toSet . fAccessFlags'

-- | Fetch the 'ConstantValue' attribute.
fConstantValue :: Field High -> Maybe (ConstantValue High)
fConstantValue =
  firstOne . faConstantValues . fAttributes

-- | Fetches the 'Signature' attribute, if any.
fSignature :: Field High -> Maybe (Signature High)
fSignature =
  firstOne . faSignatures . fAttributes

data FieldAttributes r = FieldAttributes
  { faConstantValues          :: [ ConstantValue r ]
  , faSignatures              :: [ Signature r ]
  , faVisibleAnnotations      :: [ RuntimeVisibleAnnotations r ]
  , faInvisibleAnnotations    :: [ RuntimeInvisibleAnnotations r ]
  , faVisibleTypeAnnotations      ::
      [ RuntimeVisibleTypeAnnotations FieldTypeAnnotation r ]
  , faInvisibleTypeAnnotations    ::
      [ RuntimeInvisibleTypeAnnotations FieldTypeAnnotation r ]
  , faOthers                  :: [ Attribute r ]
  }

emptyFieldAttributes :: FieldAttributes High
emptyFieldAttributes =
  FieldAttributes [] [] [] [] [] [] []

instance Staged Field where
  evolve field = label "Field" $ do
    fi <- link (fName field)
    fd <- link (fDescriptor field)
    label (Text.unpack . typeToText $ NameAndType fi fd) $ do
      fattr <- fmap (`appEndo` emptyFieldAttributes) . fromAttributes FieldAttribute (fAttributes field)
        $ collect
        [ Attr (\e a -> a {faConstantValues = e : faConstantValues a })
        , Attr (\e a -> a {faSignatures = e : faSignatures a })
        , Attr (\e a -> a {faVisibleAnnotations = e : faVisibleAnnotations a })
        , Attr (\e a -> a {faInvisibleAnnotations = e : faInvisibleAnnotations a })
        , Attr (\e a -> a {faVisibleTypeAnnotations = e : faVisibleTypeAnnotations a })
        , Attr (\e a -> a {faInvisibleTypeAnnotations = e : faInvisibleTypeAnnotations a })
        ] (\e a -> a { faOthers = e : faOthers a })
      return $ Field (fAccessFlags' field) fi fd fattr

  devolve field = do
    fi <- unlink (fName field)
    fd <- unlink (fDescriptor field)
    fattr <- fromFieldAttributes (fAttributes field)
    return $ Field (fAccessFlags' field) fi fd (SizedList fattr)

    where
      fromFieldAttributes (FieldAttributes {..}) =
        concat <$> sequence
        [ mapM toAttribute faConstantValues
        , mapM toAttribute faSignatures
        , mapM toAttribute faVisibleAnnotations
        , mapM toAttribute faInvisibleAnnotations
        , mapM toAttribute faVisibleTypeAnnotations
        , mapM toAttribute faInvisibleTypeAnnotations
        , mapM devolve faOthers
        ]

$(deriveBase ''FieldAttributes)
$(deriveBaseWithBinary ''Field)
