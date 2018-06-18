{-# LANGUAGE TemplateHaskell    #-}
{-|
Module      : Language.JVM.Field
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.JVM.Field
  ( Field (..)
  , fAccessFlags
  -- * Attributes
  , fConstantValue
  , fSignature
  , FieldAttributes (..)
  ) where


import Data.Monoid
import qualified Data.Set                as Set
import qualified Data.Text               as Text

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.Utils


-- | A Field in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5).
data Field r = Field
  { fAccessFlags'    :: !(BitSet16 FAccessFlag)
  , fName  :: !(Ref Text.Text r)
  , fDescriptor :: !(Ref FieldDescriptor r)
  , fAttributes      :: !(Attributes FieldAttributes r)
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
  { faConstantValues :: [ ConstantValue r ]
  , faSignatures     :: [ Signature r ]
  , faOthers         :: [ Attribute r ]
  }

instance Staged Field where
  evolve field = label "Field" $ do
    fi <- link (fName field)
    fd <- link (fDescriptor field)
    fattr <- fromCollector <$> fromAttributes collect' (fAttributes field)
    return $ Field (fAccessFlags' field) fi fd fattr
    where
      fromCollector (cv, sig, others) =
        FieldAttributes (appEndo cv []) (appEndo sig []) (appEndo others [])
      collect' attr =
        collect (mempty, mempty, Endo(attr:)) attr
          [ toC $ \x -> (Endo (x:), mempty, mempty)
          , toC $ \x -> (mempty, Endo (x:), mempty) ]

  devolve field = do
    fi <- unlink (fName field)
    fd <- unlink (fDescriptor field)
    fattr <- fromFieldAttributes (fAttributes field)
    return $ Field (fAccessFlags' field) fi fd (SizedList fattr)

    where
      fromFieldAttributes (FieldAttributes cvs fsg attr) =
        (\a b c -> a ++ b ++ c)
        <$> mapM toAttribute cvs
        <*> mapM toAttribute fsg
        <*> mapM devolve attr

$(deriveBase ''FieldAttributes)
$(deriveBaseWithBinary ''Field)
