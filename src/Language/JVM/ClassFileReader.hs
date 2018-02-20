{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.JVM.ClassFileReader
  (
    decodeClassFile
  , encodeClassFile
  , bootstrapDeref
  , untieClassFile
  ) where

import qualified Data.Map as Map
import Data.Word
import Control.Monad.State
import           Control.DeepSeq           (NFData)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Binary
-- -- import           Data.Binary.Get
-- -- import           Data.Binary.Put
import           Data.Monoid

-- -- import           Data.Text.Encoding        as TE
-- -- import           Data.Text.Encoding.Error  as TE

-- -- import           Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import           Data.IntMap               as IM
-- -- import qualified Data.Text                 as Text
import           GHC.Generics              (Generic)

import           Language.JVM.Constant
import           Language.JVM.ConstantPool
-- -- import           Language.JVM.Constant
import           Language.JVM.ClassFile
-- -- import           Language.JVM.TH
-- -- import           Language.JVM.Type
-- -- import           Language.JVM.Utils


-- | Create a indexed class file from a lazy 'BL.ByteString'
decodeClassFile :: BL.ByteString -> Either ClassFileError (ClassFile Index)
decodeClassFile bs = do
  case decodeOrFail bs of
    Right (_, _, cf) -> Right cf
    Left (_, _, msg) -> Left $ CFEUnreadableFile msg

-- | Create a lazy byte string from a indexed class file
encodeClassFile :: ClassFile Index -> BL.ByteString
encodeClassFile clf = do
  encode clf

-- | Changed the stage from Index to Deref
untieClassFile :: ClassFile Index -> Either ClassFileError (ClassFile Deref)
untieClassFile cf = do
  cp <- bootstrapDeref (cConstantPool cf)
  runDerefer cp (deref cf)

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

newtype Derefer a =
  Derefer (ReaderT (ConstantPool Deref) (Either ClassFileError) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader (ConstantPool Deref)
  , MonadError ClassFileError
  )

fromEither :: (msg -> ClassFileError) -> Either msg b -> Derefer b
fromEither f = either (throwError . f) return

runDerefer :: ConstantPool Deref -> Derefer a -> Either ClassFileError a
runDerefer cp (Derefer m) = runReaderT m cp

deref' :: Stage s => Stager Derefer Index s
deref' r = do
  c <- fromEither CFEPoolAccessError . access (getIndex r) =<< ask
  a <- fromConst (throwError . CFEConversionError) c
  fromDeref $ Ref (Deref ((getIndex r), a))


-- | Untie the constant pool, this requires a special operation as the constant pool
-- might reference itself.
bootstrapDeref :: ConstantPool Index -> Either ClassFileError (ConstantPool Deref)
bootstrapDeref reffed =
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
      case runDerefer (ConstantPool cp) $ deref a of
        Right c -> (IM.singleton k c, Endo id)
        Left _  -> (IM.empty, Endo ((k,a):))

deref :: Staged a => a Index -> Derefer (a Deref)
deref a =
  stage deref' a

-- $build

data CPBuilder = CPBuilder
   { cpbMapper :: Map.Map (Constant Index) Word16
   , cpbConstantPool :: ConstantPool Index
   }

stateCPBuilder
  :: (ConstantPool Index -> (a, ConstantPool Index))
  -> CPBuilder
  -> (a, CPBuilder)
stateCPBuilder f cpb =
  let (a, cp') = f . cpbConstantPool $ cpb in
  (a, cpb { cpbConstantPool = cp'})

type ContantPoolBuilder = State CPBuilder

build :: Stage s => Stager ContantPoolBuilder s Index
build r =
  case asValue r of
    Right v -> do
      c <- toConst build v
      mw <- gets (Map.lookup c . cpbMapper)
      case mw of
        Just w -> return $ Ref (Index w)
        Nothing -> do
          w <- state . stateCPBuilder $ append c
          return $ Ref (Index w)
    Left r' ->
      return r'
