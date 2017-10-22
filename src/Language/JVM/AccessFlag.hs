{-|
Module      : Language.JVM.AccessFlag
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

Contains the AccessFlags used in the different modules.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.JVM.AccessFlag
  ( MAccessFlag(..), mflags
  , FAccessFlag(..), fflags
  , CAccessFlag(..), cflags
  ) where

import           Language.JVM.Utils

import           GHC.Generics (Generic)
import           Control.DeepSeq (NFData)

-- | Access flags for the 'Language.JVM.Method.Method'
data MAccessFlag
  = MPublic
  | MPrivate
  | MProtected
  | MStatic
  | MFinal
  | MSynchronized
  | MBridge
  | MVarargs
  | MNative
  | MAbstract
  | MStrictFP
  | MSynthetic
  deriving (Ord, Show, Eq, NFData, Generic)


-- | The 'Enumish' mapping of the 'MAccessFlag'
mflags :: [(Int, MAccessFlag)]
mflags =
  [ (0,  MPublic)
  , (1,  MPrivate)
  , (2,  MProtected)
  , (3,  MStatic)
  , (4,  MFinal)
  , (5,  MSynchronized)
  , (6,  MBridge)
  , (7,  MVarargs)
  , (8,  MNative)
  , (10, MAbstract)
  , (11, MStrictFP)
  , (12, MSynthetic)
  ]

instance Enumish MAccessFlag where
  inOrder = mflags

-- | Access flags for the 'Language.JVM.ClassFile.ClassFile'
data CAccessFlag
  = CPublic
  | CFinal
  | CSuper
  | CAbstract
  | CSynthetic
  | CAnnotation
  | CEnum
  deriving (Ord, Show, Eq, NFData, Generic)

-- | The 'Enumish' mapping of the 'CAccessFlag'
cflags :: [(Int, CAccessFlag)]
cflags =
  [ (0, CPublic)
  , (4, CFinal)
  , (5, CSuper)
  , (10, CAbstract)
  , (12, CSynthetic)
  , (13, CAnnotation)
  , (14, CEnum)
  ]

instance Enumish CAccessFlag where
  inOrder = cflags

-- | Access flags for the 'Language.JVM.Field.Field'
data FAccessFlag
  = FPublic
  | FPrivate
  | FProtected
  | FStatic
  | FFinal
  | FSynchronized
  | FUnused6
  | FVolatile
  | FTransient
  | FSynthetic
  | FEnum
  deriving (Ord, Show, Eq, NFData, Generic)

-- | The 'Enumish' mapping of the 'FAccessFlag'
fflags :: [(Int, FAccessFlag)]
fflags =
  [ (0,  FPublic)
  , (1,  FPrivate)
  , (2,  FProtected)
  , (3,  FStatic)
  , (4,  FFinal)
  , (5,  FSynchronized)
  , (6,  FUnused6)
  , (7,  FVolatile)
  , (8,  FTransient)
  , (13, FSynthetic)
  , (14, FEnum)
  ]

instance Enumish FAccessFlag where
  inOrder = fflags

