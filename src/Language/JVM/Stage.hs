{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the stages, there are two stages; 'Low' and 'High'. 'Low'
represents closest to the metal and 'High' represents closer to the conceptual
representation.
-}
module Language.JVM.Stage (
  Low,
  High,
  Ref,
  Index,
  DeepRef,
  OptionalRef,
  Choice,
) where

import Data.Word

{- | Any data structure that is in the low stage should be serializable using
 the binary library.
-}
data Low

-- | Any data structure in the 'High' stage, is easier to read.
data High

{- | The basic part of the stage system is the choice. The 'Choice' chooses
 between two types depending on the stage.
-}
type family Choice a b r

type instance Choice a b High = b
type instance Choice a b Low = a

-- | An index into the constant pool.
type Index = Word16

-- | A reference is a choice between an index and a value.
type Ref v r = Choice Index v r

-- | A deep reference points to something that itself is staged.
type DeepRef v r = Ref (v r) r

-- | A reference that might point to 0
type OptionalRef t r = Choice Index (Maybe t) r
