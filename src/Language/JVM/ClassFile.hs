{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module      : Language.JVM.ClassFile
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

The class file is described in this module.
-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.JVM.ClassFile
  ( ClassFile (..)
  , cAccessFlags
  , cFields
  , cMethods
  , cSignature
  , cEnclosingMethod

  -- * Attributes
  , ClassAttributes (..)
  , cBootstrapMethods
  ) where

import           Data.Binary
import           Data.Monoid
import           Data.Set

import           Language.JVM.AccessFlag
import           Language.JVM.Attribute
import           Language.JVM.Attribute.BootstrapMethods
import           Language.JVM.Attribute.EnclosingMethod
-- import           Language.JVM.Attribute.Signature
import           Language.JVM.Constant
import           Language.JVM.ConstantPool               as CP
import           Language.JVM.Field                      (Field)
import           Language.JVM.Method                     (Method)
import           Language.JVM.Staged
import           Language.JVM.Utils

-- | A 'ClassFile' as described
-- [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html).

data ClassFile r = ClassFile
  { cMagicNumber  :: !Word32

  , cMinorVersion :: !Word16
  , cMajorVersion :: !Word16

  , cConstantPool :: !(Choice (ConstantPool r) () r)

  , cAccessFlags' :: !(BitSet16 CAccessFlag)

  , cThisClass    :: !(Ref ClassName r)
  , cSuperClass   :: !(Ref ClassName r)

  , cInterfaces   :: !(SizedList16 (Ref ClassName r))
  , cFields'      :: !(SizedList16 (Field r))
  , cMethods'     :: !(SizedList16 (Method r))
  , cAttributes   :: !(Attributes ClassAttributes r)
  }

-- | Get the set of access flags
cAccessFlags :: ClassFile r -> Set CAccessFlag
cAccessFlags = toSet . cAccessFlags'

-- | Get a list of 'Field's of a ClassFile.
cFields :: ClassFile r -> [Field r]
cFields = unSizedList . cFields'

-- | Get a list of 'Method's of a ClassFile.
cMethods :: ClassFile r -> [Method r]
cMethods = unSizedList . cMethods'


-- | Fetch the 'BootstrapMethods' attribute.
-- There can only one bootstrap methods per class, but there might not be
-- one.
cBootstrapMethods' :: ClassFile High -> Maybe (BootstrapMethods High)
cBootstrapMethods' =
  firstOne . caBootstrapMethods . cAttributes

cBootstrapMethods :: ClassFile High -> [BootstrapMethod High]
cBootstrapMethods =
  maybe [] methods . cBootstrapMethods'

cSignature :: ClassFile High -> Maybe (Signature High)
cSignature =
  firstOne . caSignature . cAttributes

cEnclosingMethod :: ClassFile High -> Maybe (EnclosingMethod High)
cEnclosingMethod =
  firstOne . caEnclosingMethod . cAttributes

data ClassAttributes r = ClassAttributes
  { caBootstrapMethods :: [ BootstrapMethods r]
  , caSignature        :: [ Signature r ]
  , caEnclosingMethod  :: [ EnclosingMethod r ]
  , caOthers           :: [ Attribute r ]
  }

instance Staged ClassFile where
  evolve cf = label "ClassFile" $ do
    tci' <- link (cThisClass cf)
    sci' <-
      if tci' /= ClassName "java/lang/Object"
      then do
        link (cSuperClass cf)
      else do
        return $ ClassName "java/lang/Object"
    cii' <- mapM link $ cInterfaces cf
    cf' <- mapM evolve $ cFields' cf
    cm' <- mapM evolve $ cMethods' cf
    ca' <- fromCollector <$> fromAttributes collect' (cAttributes cf)
    return $ cf
      { cConstantPool = ()
      , cThisClass = tci'
      , cSuperClass = sci'
      , cInterfaces = cii'
      , cFields'            = cf'
      , cMethods'           = cm'
      , cAttributes         = ca'
      }
    where
      fromCollector = flip appEndo (ClassAttributes [] [] [] [])
      collect' attr =
        collect (Endo (\ca -> ca {caOthers = attr: caOthers ca})) attr
          [ toC $ \e -> Endo (\ca -> ca {caSignature = e : caSignature ca})
          , toC $ \e -> Endo (\ca -> ca {caEnclosingMethod = e : caEnclosingMethod ca})
          , toC $ \e -> Endo (\ca -> ca {caBootstrapMethods = e : caBootstrapMethods ca})
          ]

  devolve cf = do
    tci' <- unlink (cThisClass cf)
    sci' <-
      if cThisClass cf /= ClassName "java/lang/Object" then
        unlink (cSuperClass cf)
      else
        return $ 0
    cii' <- mapM unlink $ cInterfaces cf
    cf' <- mapM devolve $ cFields' cf
    cm' <- mapM devolve $ cMethods' cf
    ca' <- fromClassAttributes $ cAttributes cf
    return $ cf
      { cConstantPool       = CP.empty
      -- We cannot yet set the constant pool
      , cThisClass = tci'
      , cSuperClass = sci'
      , cInterfaces  = cii'
      , cFields'            = cf'
      , cMethods'           = cm'
      , cAttributes         = SizedList ca'
      }
    where
      fromClassAttributes (ClassAttributes cm cs cem at) = do
        cm' <- mapM toAttribute cm
        cs' <- mapM toAttribute cs
        cem' <- mapM toAttribute cem
        at' <- mapM devolve at
        return (cm' ++ cs' ++ cem' ++ at')

$(deriveBase ''ClassAttributes)
$(deriveBaseWithBinary ''ClassFile)
