{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
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

  , Ref (..)

  , Index
  , idx
  , value

  , DeepRef (..)
  , deepValue
  , deepRef
  , Choice
  ) where

import GHC.Generics
import Control.DeepSeq
import Data.Binary

data Low
data High

data family Ref v r
data instance Ref v High = RefV v
data instance Ref v Low = RefI Word16

deriving instance Show (Ref v Low)
deriving instance NFData (Ref v Low)
deriving instance Generic (Ref v Low)
deriving instance Eq (Ref v Low)

deriving instance Ord (Ref v Low)

deriving instance Show v => Show (Ref v High)
deriving instance NFData v => NFData (Ref v High)
deriving instance Generic (Ref v High)
deriving instance Eq v => Eq (Ref v High)


type family Choice r a b
type instance Choice High a b = b
type instance Choice Low a b = a

type Index = Word16

value :: Ref v High -> v
value (RefV v) = v
{-# INLINE value #-}

deepValue :: DeepRef v High -> v High
deepValue (DeepRef (RefV v)) = v
{-# INLINE deepValue #-}

instance Binary (Ref a Low) where
  get = RefI <$> get
  put = put . idx

idx :: Ref a Low -> Word16
idx (RefI w) = w
{-# INLINE idx #-}

newtype DeepRef v r = DeepRef { unDeep :: (Ref (v r) r) }

deepRef :: v High -> DeepRef v High
deepRef v = DeepRef (RefV v)

deriving instance Show (DeepRef v Low)
deriving instance NFData (DeepRef v Low)
deriving instance Generic (DeepRef v Low)
deriving instance Eq (DeepRef v Low)

deriving instance Show (v High) => Show (DeepRef v High)
deriving instance NFData (v High) => NFData (DeepRef v High)
deriving instance Generic (DeepRef v High)
deriving instance Eq (v High) => Eq (DeepRef v High)

deriving instance Ord (DeepRef v Low)
deriving instance Binary (DeepRef v Low)
