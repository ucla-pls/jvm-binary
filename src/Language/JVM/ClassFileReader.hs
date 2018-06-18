{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.JVM.ClassFileReader
  ( readClassFile
  , writeClassFile

  , writeClassFile'

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

-- | Decode a class file from a lazy 'BL.ByteString'. Ensures that the lazy
-- bytestring is read to EOF, and thereby closing any open files.
decodeClassFile :: BL.ByteString -> Either ClassFileError (ClassFile Low)
decodeClassFile bs = do
  case decodeOrFail bs of
    Right (rest, off, cf)
      | BL.length rest == 0 -> Right cf
      | otherwise ->
        unreadable rest off "expected end of file"
    Left (rest, off, msg) ->
      unreadable rest off msg
  where
    unreadable rest off msg =
      Left $ CFEUnreadableFile ((show off) ++ "/" ++ (show $ BL.length rest) ++ ": " ++ msg)

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

-- | Top level command that combines 'devolve' and 'encode', but tries
-- to retain exact syntax of a previous run using the class pool.
writeClassFile' :: ConstantPool Low -> ClassFile High -> BL.ByteString
writeClassFile' cp =
  encodeClassFile . devolveClassFile' cp


-- $deref
-- Dereffing is the flattening of the constant pool to get the values
-- of all references.

-- | An error while reading a class file is represented using
-- this data structure
data ClassFileError
  = CFEPoolAccessError !String !PoolAccessError
  | CFEInconsistentClassPool !String !String
  | CFEConversionError !String !String
  | CFEUnreadableFile !String
  deriving (Show, Eq, Generic)

instance NFData ClassFileError

newtype Evolve a =
  Evolve (ReaderT (String, ConstantPool High) (Either ClassFileError) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader (String, ConstantPool High)
  , MonadError ClassFileError
  )

runEvolve :: ConstantPool High -> Evolve a -> Either ClassFileError a
runEvolve cp (Evolve m) = runReaderT m ("", cp)

instance LabelM Evolve where
  label str (Evolve m) = do
    Evolve . withReaderT (\(x, cp) -> (x ++ "/" ++ str, cp)) $ m

instance EvolveM Evolve where
  link w = do
    (lvl, cp) <- ask
    r <- either (throwError . CFEPoolAccessError lvl ) return $ access w cp
    fromConst (throwError . CFEInconsistentClassPool lvl) r

  attributeError msg = do
    (lvl, _) <- ask
    throwError (CFEConversionError lvl msg)

-- | Untie the constant pool, this requires a special operation as the constant pool
-- might reference itself.
bootstrapConstantPool :: ConstantPool Low -> Either ClassFileError (ConstantPool High)
bootstrapConstantPool reffed =
  case stage' IM.empty (IM.toList $ unConstantPool reffed) of
    Right cp ->
      Right $ ConstantPool cp
    Left xs ->
      Left . CFEInconsistentClassPool "ConstantPool"
           $ "Could not load all constants in the constant pool: " ++ (show xs)
  where
    stage' cp mis =
      let (cp', mis') = foldMap (grow cp) mis in
      case appEndo mis' [] of
        [] | IM.null cp' -> Right cp
        xs
          | IM.null cp' ->
            Left xs
          | otherwise ->
            stage' (cp `IM.union` cp') (map snd xs)

    grow cp (k,a) =
      case runEvolve (ConstantPool cp) $ evolve a of
        Right c -> (IM.singleton k c, Endo id)
        Left msg  -> (IM.empty, Endo ((msg, (k,a)):))

-- $build

data CPBuilder = CPBuilder
   { cpbMapper       :: Map.Map (Constant Low) Index
   , cpbConstantPool :: ConstantPool Low
   }

cpbEmpty :: CPBuilder
cpbEmpty = CPBuilder Map.empty CP.empty

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

instance LabelM ConstantPoolBuilder

instance DevolveM ConstantPoolBuilder where
  unlink r = do
    c <- toConst r
    c' <- devolve c
    mw <- gets (Map.lookup c' . cpbMapper)
    case mw of
      Just w -> return w
      Nothing -> do
        w <- state . stateCPBuilder $ c'
        return w

stateCPBuilder
  :: Constant Low
  -> CPBuilder
  -> (Word16, CPBuilder)
stateCPBuilder c' cpb =
  let (w, cp') = append c' . cpbConstantPool $ cpb in
  (w, cpb { cpbConstantPool = cp'
          , cpbMapper = Map.insert c' w . cpbMapper $ cpb
          })
