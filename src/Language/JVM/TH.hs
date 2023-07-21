{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
Module      : Language.JVM.TH
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains some Template Haskell functions for internal use.
-}
module Language.JVM.TH (
  derives,
  deriveBase,
  deriveAll,
  deriveEach,
  bases,
  binary,
  highOrd,
  deriveBases,
  deriveThese,
  deriveBaseWithBinary,
  deriveBasesWithBinary,
) where

import Language.Haskell.TH

import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.Foldable (fold)
import GHC.Generics

import Language.JVM.Stage

{- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
 from something that is 'Staged'
-}
deriveThese :: Name -> [Name] -> Q [Dec]
deriveThese name items =
  return . concat $ do
    x <- ConT <$> items
    return
      [ StandaloneDerivD Nothing [] (AppT x (AppT n (ConT ''High)))
      , StandaloneDerivD Nothing [] (AppT x (AppT n (ConT ''Low)))
      ]
 where
  n = ConT name

data Deriver = Deriver
  { instanceName :: Name
  , instancePrerecusives :: [Name]
  , strategy :: Maybe DerivStrategy
  , level :: Name
  }

{- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
 from something that is 'Staged'
-}
derives :: Deriver -> Name -> Q [Dec]
derives Deriver{..} name = do
  tpe <- reify name
  arity <- case tpe of
    TyConI (DataD [] _ lst Nothing _ _) -> pure $ length lst
    TyConI (NewtypeD [] _ lst Nothing _ _) -> pure $ length lst
    t -> fail ("unexpected type: " ++ show t)

  case arity of
    0 -> do
      fail "expected this to be a staged file"
    x -> do
      names <- replicateM (x - 1) (newName "r")
      return $
        [ StandaloneDerivD
            strategy
            [ AppT (ConT p) (AppT (VarT m) (ConT level))
            | m <- names
            , p <- instancePrerecusives
            ]
            (AppT (ConT instanceName) (AppT (foldl (\a b -> AppT a (VarT b)) n names) (ConT level)))
        ]
 where
  n = ConT name

bases :: [Deriver]
bases =
  fold
    [ [ Deriver ''Show [''Show] (Just StockStrategy) lvl
      , Deriver ''Eq [''Eq] (Just StockStrategy) lvl
      , Deriver ''Generic [''Generic] (Just StockStrategy) lvl
      , Deriver ''NFData [''NFData, ''Generic] (Just AnyclassStrategy) lvl
      ]
    | lvl <- [''High, ''Low]
    ]
    ++ [Deriver ''Ord [''Ord] (Just StockStrategy) ''Low]

highOrd :: Deriver
highOrd = Deriver ''Ord [''Ord] (Just StockStrategy) ''High

binary :: Deriver
binary =
  Deriver ''Binary [''Binary, ''Generic] (Just AnyclassStrategy) ''Low

{- | Derives the 'NFData', 'Show', 'Eq', and 'Generic'
 from something that is 'Staged'
-}
deriveBase :: Name -> Q [Dec]
deriveBase name =
  concat
    <$> sequence
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
 where
  n = conT name

-- -- | Derives the bases of a list of names
-- deriveAll :: [Deriver] -> [Name] -> Q [Dec]
-- deriveAll names =
--   fmap concat . mapM (deriveEach names)

-- | Derives the bases of a list of names
deriveAll :: [([Name], [Deriver])] -> Q [Dec]
deriveAll pairs =
  fmap concat . forM pairs $ \(p, drv) -> do
    concat <$> mapM (deriveEach drv) p

-- | Derives the bases of a list of names
deriveEach :: [Deriver] -> Name -> Q [Dec]
deriveEach names n =
  fmap concat $ mapM (`derives` n) names

-- | Derives the bases of a list of names
deriveBases :: [Name] -> Q [Dec]
deriveBases names =
  concat <$> mapM deriveBase names

-- | Derives the bases of a list of names
deriveBasesWithBinary :: [Name] -> Q [Dec]
deriveBasesWithBinary names =
  concat <$> mapM deriveBaseWithBinary names

{- | Derives the 'NFData', 'Show', 'Eq', and 'Generic' from something that is
 'Staged'
-}
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
