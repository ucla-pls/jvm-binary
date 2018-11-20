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

  -- * Helpers
  , roundtripCopy

  -- * Evolve
  , Evolve
  , ClassFileError
  , EvolveConfig (..)
  , runEvolve
  , bootstrapConstantPool

  -- * Builder
  , ConstantPoolBuilder
  , runConstantPoolBuilder
  , CPBuilder (..)
  , builderFromConstantPool
  , cpbEmpty
  ) where

import           Control.DeepSeq (NFData)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Monoid
import           GHC.Generics (Generic)

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

-- | Evolve the class file to inline the references. A filter function is
-- provided to remove some attributes. This will sometimes give faster loading
-- times.
evolveClassFile ::
  ((AttributeLocation, Text.Text) -> Bool)
  -> ClassFile Low
  -> Either ClassFileError (ClassFile High)
evolveClassFile fn cf = do
  cp <- bootstrapConstantPool (cConstantPool cf)
  runEvolve (EvolveConfig [] cp fn ) (evolve cf)

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
  evolveClassFile (const True) clf

-- | Top level command that combines 'devolve' and 'encode'.
writeClassFile :: ClassFile High -> BL.ByteString
writeClassFile =
  encodeClassFile . devolveClassFile

-- | Top level command that combines 'devolve' and 'encode', but tries
-- to retain exact syntax of a previous run using the class pool.
writeClassFile' :: ConstantPool Low -> ClassFile High -> BL.ByteString
writeClassFile' cp =
  encodeClassFile . devolveClassFile' cp


-- | A test function, essentially reading the classfile and then writing it
-- to another file.
roundtripCopy :: FilePath -> FilePath -> IO ()
roundtripCopy f1 f2 = do
  Right cf <- readClassFile <$> BL.readFile f1
  BL.writeFile f2 $ writeClassFile cf

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

data EvolveConfig =
  EvolveConfig
  { ecLabel :: [String]
  , ecConstantPool :: ConstantPool High
  , ecAttributeFilter :: ((AttributeLocation, Text.Text) -> Bool)
  }

newtype Evolve a =
  Evolve (ReaderT EvolveConfig (Either ClassFileError) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader EvolveConfig
  , MonadError ClassFileError
  )

runEvolve :: EvolveConfig -> Evolve a -> Either ClassFileError a
runEvolve ev (Evolve m) = runReaderT m ev

instance LabelM Evolve where
  label str (Evolve m) = do
    Evolve . withReaderT (\ec -> ec { ecLabel = str : ecLabel ec}) $ m

showLvl :: [String] -> String
showLvl = List.intercalate "/" . reverse

instance EvolveM Evolve where
  link w = do
    ec <- ask
    let lvl = showLvl ( ecLabel ec )
    r <- either (throwError . CFEPoolAccessError lvl)  return $ access w (ecConstantPool ec)
    fromConst (throwError . CFEInconsistentClassPool lvl) r

  attributeFilter =
    asks ecAttributeFilter

  attributeError msg = do
    lvl <- asks (showLvl . ecLabel)
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
      let (cp', mis') =
            foldMap (grow (EvolveConfig [] (ConstantPool cp) (const True))) mis
      in if IM.null cp'
        then
          case appEndo mis' [] of
            [] -> Right cp
            xs -> Left xs
        else
          stage' (cp `IM.union` cp') . map snd $ appEndo mis' []

    grow :: EvolveConfig -> (Int, Constant Low) -> (IM.IntMap (Constant High), Endo [(ClassFileError, (Int, Constant Low))])
    grow ec (k,a) =
      case runEvolve ec (evolve a) of
        Right c -> (IM.singleton k c, Endo id)
        Left msg  -> (IM.empty, Endo ((msg, (k,a)):))

{-# SCC bootstrapConstantPool #-}

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
runConstantPoolBuilder (ConstantPoolBuilder m) a =
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
