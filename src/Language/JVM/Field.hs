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
  , fName
  , fDescriptor
  , fAccessFlags
  -- * Attributes
  , fConstantValue
  , FieldAttributes (..)
  ) where


import Data.Monoid
import qualified Data.Set                as Set
import qualified Data.Text               as Text

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Constant
import           Language.JVM.Stage
import           Language.JVM.Utils


-- | A Field in the class-file, as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5).
data Field r = Field
  { fAccessFlags'    :: BitSet16 FAccessFlag
  , fNameIndex       :: Ref Text.Text r
  , fDescriptorIndex :: Ref FieldDescriptor r
  , fAttributes      :: Choice r (SizedList16 (Attribute r)) (FieldAttributes r)
  }

-- | Get the name of the field
fName :: Field High -> Text.Text
fName = valueF fNameIndex

-- | Get the set of access flags
fAccessFlags :: Field r -> Set.Set FAccessFlag
fAccessFlags = toSet . fAccessFlags'

-- | Get the descriptor of the field
fDescriptor :: Field High -> FieldDescriptor
fDescriptor = valueF fDescriptorIndex

-- | Fetch the 'ConstantValue' attribute.
fConstantValue :: Field High -> Maybe (ConstantValue High)
fConstantValue =
  firstOne . faConstantValues . fAttributes

data FieldAttributes r = FieldAttributes
  { faConstantValues :: [ ConstantValue r ]
  , faOthers         :: [ Attribute r ]
  }

instance Staged Field where
  evolve field = do
    fi <- evolve (fNameIndex field)
    fd <- evolve (fDescriptorIndex field)
    fattr <- fromCollector <$> fromAttributes collect' (fAttributes field)
    return $ Field (fAccessFlags' field) fi fd fattr
    where
      fromCollector (cv, others) =
        FieldAttributes (appEndo cv []) (appEndo others [])
      collect' attr =
        collect (mempty, Endo(attr:)) attr
          [ toC $ \x -> (Endo (x:), mempty) ]

  devolve field = do
    fi <- devolve (fNameIndex field)
    fd <- devolve (fDescriptorIndex field)
    fattr <- fromFieldAttributes (fAttributes field)
    return $ Field (fAccessFlags' field) fi fd (SizedList fattr)

    where
      fromFieldAttributes (FieldAttributes cvs attr) =
        (++) <$> mapM toAttribute cvs <*> mapM devolve attr

$(deriveBase ''FieldAttributes)
$(deriveBaseWithBinary ''Field)
