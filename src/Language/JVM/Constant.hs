{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
module Language.JVM.Constant
  ( Constant (..)
  , ConstantRef (..)
  , ConstantPool (..)
  , poolSize

  , lookupConstant
  , lookupText

  , toListOfConstants
  ) where

import           GHC.Generics           (Generic)

import           Prelude                hiding (fail, lookup)

import           Control.Monad          (forM_)
import           Control.Monad.Fail     (fail)

import qualified Data.ByteString        as BS
import qualified Data.IntMap.Strict     as IM
import           Data.Word
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put


import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as TE

newtype ConstantRef =
  ConstantRef Word16
  deriving (Eq, Show, Generic)

instance Binary ConstantRef where

data Constant
  = String !BS.ByteString
  | Integer !Word32
  | Float !Word32
  | Long !Word64
  | Double !Word64
  | ClassRef !ConstantRef
  | StringRef !ConstantRef
  | FieldRef !ConstantRef !ConstantRef
  | MethodRef !ConstantRef !ConstantRef
  | InterfaceMethodRef !ConstantRef !ConstantRef
  | NameAndType !ConstantRef !ConstantRef
  | MethodHandle !Word8 !ConstantRef
  | MethodType !ConstantRef
  | InvokeDynamic !ConstantRef !ConstantRef
  deriving (Show, Eq)

instance Binary Constant where
  get = do
    ident <- getWord8
    case ident of
      1  -> do len <- getInt16be; String <$> getByteString (fromIntegral len)
      3  -> Integer <$> get
      4  -> Float <$> get
      5  -> Long <$> get
      6  -> Double <$> get
      7  -> ClassRef <$> get
      8  -> StringRef <$> get
      9  -> FieldRef <$> get <*> get
      10 -> MethodRef <$> get <*> get
      11 -> InterfaceMethodRef <$> get <*> get
      12 -> NameAndType <$> get <*> get
      15 -> MethodHandle <$> get <*> get
      16 -> MethodType <$> get
      18 -> InvokeDynamic <$> get <*> get
      _  -> fail $ "Unkown identifier " ++ show ident

  put x =
    case x of
      String bs -> do
        putWord8 1
        putInt16be . fromIntegral $ BS.length bs
        putByteString bs
      Integer i              -> do putWord8 3; put i
      Float i                -> do putWord8 4; put i
      Long i                 -> do putWord8 5; put i
      Double i               -> do putWord8 6; put i
      ClassRef i             -> do putWord8 7; put i
      StringRef i            -> do putWord8 8; put i
      FieldRef i j           -> do putWord8 9; put i; put j
      MethodRef i j          -> do putWord8 10; put i; put j
      InterfaceMethodRef i j -> do putWord8 11; put i; put j
      NameAndType i j        -> do putWord8 12; put i; put j
      MethodHandle i j       -> do putWord8 15; put i; put j
      MethodType i           -> do putWord8 16; put i;
      InvokeDynamic i j      -> do putWord8 18; put i; put j

newtype ConstantPool = ConstantPool
  { unConstantPool :: IM.IntMap Constant
  } deriving (Show, Eq)

toListOfConstants :: ConstantPool -> [(ConstantRef, Constant)]
toListOfConstants =
  map (\(a,b) -> (ConstantRef . fromIntegral $ a, b)) . IM.toList . unConstantPool

poolSize :: Constant -> Int
poolSize x =
  case x of
    Double _ ->  2
    Long _   -> 2
    _        -> 1

instance Binary ConstantPool where
  get = do
    len <- fromIntegral <$> getInt16be
    list <- go len 1
    return . ConstantPool $ IM.fromList list
    where
      go len n | len > n = do
        constant <- get
        rest <- go len (n + poolSize constant)
        return $ (n, constant) : rest
      go _ _ = return []
  put (ConstantPool p)= do
    case IM.maxViewWithKey p of
      Just ((key, e), _) -> do
        putInt16be (fromIntegral (key + poolSize e))
        forM_ (IM.toAscList p) (put . snd)
      Nothing -> do
        putInt16be 0

lookupConstant :: ConstantRef -> ConstantPool -> Maybe Constant
lookupConstant (ConstantRef ref) (ConstantPool cp) =
  IM.lookup (fromIntegral ref) cp

lookupText :: ConstantRef -> ConstantPool -> Maybe Text.Text
lookupText ref cp = do
  String str <- lookupConstant ref cp
  case TE.decodeUtf8' str of
    Left _    -> Nothing
    Right txt -> Just txt
