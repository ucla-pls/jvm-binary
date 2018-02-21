{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.JVM.TH
  ( deriveBase
  , deriveBases
  , deriveBaseWithBinary
  , Low
  , High
  ) where

import Language.Haskell.TH

import GHC.Generics
import Control.DeepSeq
import Data.Binary


data Low
data High

-- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
-- from something that is 'Staged'
deriveBase :: Name -> Q [Dec]
deriveBase name =
  concat <$> sequence
  [ [d|deriving instance Show ($n Low)|]
  , [d|deriving instance Eq ($n Low)|]
  , [d|deriving instance Generic ($n Low)|]
  , [d|deriving instance NFData ($n Low)|]
  , [d|deriving instance Ord ($n Low)|]

  , [d|deriving instance Show ($n High)|]
  , [d|deriving instance Eq ($n High)|]
  , [d|deriving instance Generic ($n High)|]
  , [d|deriving instance NFData ($n High)|]
  ]
  where n = conT name

deriveBases :: [Name] -> Q [Dec]
deriveBases names =
  concat <$> mapM deriveBase names

-- | Derives the 'NFData', 'Show', 'Eq', and 'Generic' from something that is
-- 'Staged'
deriveBaseWithBinary :: Name -> Q [Dec]
deriveBaseWithBinary name = do
  b <- deriveBase name
  m1 <- deriveBinary name
  return (b ++ m1)

deriveBinary :: Name -> Q [Dec]
deriveBinary name =
  [d|deriving instance Binary ($n Low)|]
  where
    n = conT name

-- -- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
-- -- from something that is 'Staged'
-- deriveBaseO :: Name -> Name -> Q [Dec]
-- deriveBaseO tp name = do
--   b <- deriveBase name
--   m1 <- [d|deriving instance Eq ($n $t)|]
--   m2 <- [d|deriving instance Ord ($n $t)|]
--   return (b ++ m1 ++ m2)
--   where
--     n = conT name
--     t = conT tp

-- -- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
-- -- from something that is 'Staged'
-- deriveBaseBO :: Name -> Name -> Q [Dec]
-- deriveBaseBO tp name = do
--   b <- deriveBaseB tp name
--   m1 <- [d|deriving instance Eq ($n $t)|]
--   m2 <- [d|deriving instance Ord ($n $t)|]
--   return (b ++ m1 ++ m2)
--   where
--     n = conT name
--     t = conT tp
