{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.JVM.TH
  ( deriveBase
  , deriveBaseB
  , deriveBaseO
  , deriveBaseBO
  , deriveShow1
  , deriveEq1

  , Eq1
  , Show1
  , Generic
  , Generic1
  , NFData
  , NFData1
  ) where

import Language.Haskell.TH
import Data.Functor.Classes (Eq1, Show1)
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq (NFData, NFData1)

import Data.Binary
import           Text.Show.Deriving
import           Data.Eq.Deriving

-- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
-- from something that is 'Staged'
deriveBase :: Name -> Q [Dec]
deriveBase name = do
  m1 <- [d|deriving instance Show1 r => Show ($n r)|]
  m2 <- [d|deriving instance Eq1 r => Eq ($n r)|]
  m3 <- [d|deriving instance Generic1 r => Generic ($n r)|]
  m4 <- [d|deriving instance (NFData1 r, Generic1 r) => NFData ($n r)|]
  return (m1 ++ m2 ++ m3 ++ m4)
  where n = conT name

-- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
-- from something that is 'Staged'
deriveBaseB :: Name -> Name -> Q [Dec]
deriveBaseB tp name = do
  b <- deriveBase name
  m1 <- [d|deriving instance Binary ($n $t)|]
  return (b ++ m1)
  where
    n = conT name
    t = conT tp

-- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
-- from something that is 'Staged'
deriveBaseO :: Name -> Name -> Q [Dec]
deriveBaseO tp name = do
  b <- deriveBase name
  m1 <- [d|deriving instance Eq ($n $t)|]
  m2 <- [d|deriving instance Ord ($n $t)|]
  return (b ++ m1 ++ m2)
  where
    n = conT name
    t = conT tp

-- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
-- from something that is 'Staged'
deriveBaseBO :: Name -> Name -> Q [Dec]
deriveBaseBO tp name = do
  b <- deriveBaseB tp name
  m1 <- [d|deriving instance Eq ($n $t)|]
  m2 <- [d|deriving instance Ord ($n $t)|]
  return (b ++ m1 ++ m2)
  where
    n = conT name
    t = conT tp
