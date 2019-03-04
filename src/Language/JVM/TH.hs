{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Language.JVM.TH
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains some Template Haskell functions for internal use.
-}

module Language.JVM.TH
  ( deriveBase
  , deriveBases
  , deriveThese
  , deriveBaseWithBinary
  ) where

import Language.Haskell.TH

import GHC.Generics
import Control.DeepSeq
import Data.Binary

import Language.JVM.Stage


-- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
-- from something that is 'Staged'
deriveThese :: Name -> [Name] -> Q [Dec]
deriveThese name items =
  return . concat $ do
  x <- ConT <$> items
  return
    [ StandaloneDerivD Nothing [] (AppT x (AppT n (ConT ''High)))
    , StandaloneDerivD Nothing [] (AppT x (AppT n (ConT ''Low)))
    ]
  where n = ConT name

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

-- | Derives the bases of a list of names
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
