{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.JVM.ClassFileReader
  ( readClassFile
  , writeClassFile

  -- * Finer granularity commands
  , decodeClassFile
  , encodeClassFile
  , evolveClassFile
  , devolveClassFile
  , devolveClassFile'

  -- * Evolve
  , Evolve
  , ClassFileError
  , runEvolve
  , bootstrapConstantPool

  -- * Builder
  , ConstantPoolBuilder
  , runConstantPoolBuilder
  , CPBuilder (..)
  , builderFromConstantPool
  , cpbEmpty
  ) where

import           Control.DeepSeq           (NFData)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Lazy      as BL
import qualified Data.IntMap               as IM
import qualified Data.Map                  as Map
import           Data.Monoid
import           Data.Binary
import           GHC.Generics              (Generic)

import           Language.JVM.ClassFile
import           Language.JVM.Constant
import           Language.JVM.ConstantPool as CP
import           Language.JVM.Staged

-- | Decode a class file from a lazy 'BL.ByteString'
decodeClassFile :: BL.ByteString -> Either ClassFileError (ClassFile Low)
decodeClassFile bs = do
  case decodeOrFail bs of
    Right (_, _, cf) -> Right cf
    Left (_, _, msg) -> Left $ CFEUnreadableFile msg

-- | Create a lazy byte string from a class file
encodeClassFile :: ClassFile Low -> BL.ByteString
encodeClassFile clf = do
  encode clf

-- | Changed the stage from Index to Deref
evolveClassFile :: ClassFile Low -> Either ClassFileError (ClassFile High)
evolveClassFile cf = do
  cp <- bootstrapConstantPool (cConstantPool cf)
  runEvolve cp (evolve cf)

-- | Devolve a ClassFile from High to Low. This might make the 'ClassFile' contain
-- invalid attributes, since we can't read all attributes. If this this is a problem
-- see 'devolveClassFile''.
devolveClassFile :: ClassFile High -> ClassFile Low
devolveClassFile cf =
  let (cf', cpb) = runConstantPoolBuilder (devolve cf) cpbEmpty in
  cf' { cConstantPool = cpbConstantPool cpb }

-- | Devolve a 'ClassFile' form 'High' to 'Low', while maintaining the class
-- pool of the original class file. This is useful if we care that unread
-- attributes are still valid. This can cause untended bloat as we do not
-- want to throw away anything in the program
devolveClassFile' :: ConstantPool Low -> ClassFile High -> ClassFile Low
devolveClassFile' cp cf =
  let (cf', cpb) = runConstantPoolBuilder (devolve cf) (builderFromConstantPool cp) in
  cf' { cConstantPool = cpbConstantPool cpb }


-- | Top level command that combines 'decode' and 'evolve'.
readClassFile :: BL.ByteString -> Either ClassFileError (ClassFile High)
readClassFile bs = do
  clf <- decodeClassFile bs
  evolveClassFile clf

-- | Top level command that combines 'devolve' and 'encode'.
writeClassFile :: ClassFile High -> BL.ByteString
writeClassFile =
  encodeClassFile . devolveClassFile


-- $deref
-- Dereffing is the flattening of the constant pool to get the values
-- of all references.

-- | An error while reading a class file is represented using
-- this data structure
data ClassFileError
  = CFEPoolAccessError !PoolAccessError
  | CFEInconsistentClassPool !String
  | CFEConversionError !String
  | CFEUnreadableFile !String
  deriving (Show, Eq, Generic)

instance NFData ClassFileError

newtype Evolve a =
  Evolve (ReaderT (ConstantPool High) (Either ClassFileError) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader (ConstantPool High)
  , MonadError ClassFileError
  )

runEvolve :: ConstantPool High -> Evolve a -> Either ClassFileError a
runEvolve cp (Evolve m) = runReaderT m cp

instance EvolveM Evolve where
  link w = do
    r' <- reader (access w)
    r <- either (throwError . CFEPoolAccessError) return r'
    fromConst (throwError . CFEInconsistentClassPool) r

  attributeError msg = do
    throwError (CFEConversionError msg)

-- | Untie the constant pool, this requires a special operation as the constant pool
-- might reference itself.
bootstrapConstantPool :: ConstantPool Low -> Either ClassFileError (ConstantPool High)
bootstrapConstantPool reffed =
  case stage' (IM.empty, IM.toList $ unConstantPool reffed) of
    (cp, []) ->
      Right $ ConstantPool cp
    (_, _:_) ->
      Left (CFEInconsistentClassPool
            "Could not load all constants in the constant pool")
  where
    stage' (cp, mis) =
      if IM.null cp'
      then (cp, mis)
      else stage' (cp `IM.union` cp', appEndo mis' [])
      where (cp', mis') = foldMap (grow cp) mis

    grow cp (k,a) =
      case runEvolve (ConstantPool cp) $ evolve a of
        Right c -> (IM.singleton k c, Endo id)
        Left _  -> (IM.empty, Endo ((k,a):))

-- $build

data CPBuilder = CPBuilder
   { cpbMapper       :: Map.Map (Constant Low) Index
   , cpbConstantPool :: ConstantPool Low
   }

cpbEmpty :: CPBuilder
cpbEmpty = CPBuilder Map.empty CP.empty

stateCPBuilder
  :: (ConstantPool Low -> (a, ConstantPool Low))
  -> CPBuilder
  -> (a, CPBuilder)
stateCPBuilder f cpb =
  let (a, cp') = f . cpbConstantPool $ cpb in
  (a, cpb { cpbConstantPool = cp'})

builderFromConstantPool :: ConstantPool Low -> CPBuilder
builderFromConstantPool cp =
  CPBuilder (Map.fromList . map change . IM.toList $ unConstantPool cp) cp
  where
    change (a, b) = (b, fromIntegral a)

newtype ConstantPoolBuilder a =
  ConstantPoolBuilder (State CPBuilder a)
  deriving (Monad, MonadState CPBuilder, Functor, Applicative)

runConstantPoolBuilder :: ConstantPoolBuilder a -> CPBuilder -> (a, CPBuilder)
runConstantPoolBuilder (ConstantPoolBuilder m) a=
  runState m a

instance DevolveM ConstantPoolBuilder where
  unlink r = do
    c <- toConst r
    c' <- devolve c
    mw <- gets (Map.lookup c' . cpbMapper)
    case mw of
      Just w -> return w
      Nothing -> do
        w <- state . stateCPBuilder $ append c'
        return w
