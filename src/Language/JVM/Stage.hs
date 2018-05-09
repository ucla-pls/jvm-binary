{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-|
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains the stages, there are two stages; 'Low' and 'High'. 'Low'
represents closest to the metal and 'High' represents closer to the conceptual
representation.
-}
module Language.JVM.Stage
  ( Low
  , High
  , Ref
  , Index
  , DeepRef
  , Choice
  ) where

import Data.Binary

data Low
data High

type family Choice r a b
type instance Choice High a b = b
type instance Choice Low a b = a
type Ref v r = Choice r Index v
type Index = Word16

type DeepRef v r = Ref (v r) r
