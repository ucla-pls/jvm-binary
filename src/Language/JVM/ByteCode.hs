{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- {-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-|
Module      : Language.JVM.ByteCode
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}
module Language.JVM.ByteCode
  ( ByteCode (..)
  , unByteCode

  -- * evolve and devolve
  , evolveByteCode
  , devolveByteCode
  , evolveOffset
  , devolveOffset

  , ByteCodeStaged (..)

  -- * Managing offsets
  , ByteCodeInst (..)
  , ByteCodeRef
  , ByteCodeOffset
  , ByteCodeIndex
  , OffsetMap
  , indexOffset
  , offsetIndex
  , offsetMap

  , generateOffsets

  -- * ByteCode Operations
  , ByteCodeOpr (..)

  , CConstant (..)
  , OneOrTwo (..)

  , SwitchTable (..)
  , switchHigh

  , FieldAccess (..)
  , Invocation (..)

  -- * Operations
  , BinOpr (..)
  , BitOpr (..)
  , CmpOpr (..)
  , CastOpr (..)

  -- * Type sets
  , ArithmeticType (..)
  , SmallArithmeticType (..)
  , LocalType (..)
  , ArrayType (..)

  -- * Renames
  , WordSize
  , ByteOffset
  , LocalAddress
  , IncrementAmount
  ) where

import           GHC.Generics          (Generic)

import           Numeric               (showHex)
import           Prelude               hiding (fail)

import           Control.DeepSeq       (NFData)
import           Control.Monad         hiding (fail)
import           Control.Monad.Fail    (fail)
import           Unsafe.Coerce

import           Data.Binary
import           Data.Binary.Get       hiding (Get, label)
import           Data.Binary.Put       hiding (Put)
import qualified Data.ByteString.Lazy  as BL
import           Data.Int
import qualified Data.IntMap.Strict           as IM
import qualified Data.Vector           as V

import           Language.JVM.Constant
import           Language.JVM.Staged
import           Language.JVM.Type

-- | ByteCode constains a list of ByteCode instructions and the size of the bytecode.
-- if the ByteCode is in the Low stage then the byte code instructions are
-- annotated with the byte code offsets.
data ByteCode i = ByteCode
  { byteCodeSize :: !Word32
  , byteCodeInstructions :: V.Vector (ByteCodeInst i)
  }

unByteCode :: ByteCode i -> V.Vector (ByteCodeInst i)
unByteCode = byteCodeInstructions

-- | The offset in the byte code
type ByteCodeOffset = Word16

-- | The index of the byte code.
type ByteCodeIndex = Int

-- | A ByteCode reference is either byte code offset in the
-- low stage, and a byte code index in the high state
type ByteCodeRef i  = Choice ByteCodeOffset ByteCodeIndex i

-- | The offset map, maps offset to instruction ids.
type OffsetMap = IM.IntMap ByteCodeIndex

-- | Given an `OffsetMap` turn a offset into a bytecode index
offsetIndex :: OffsetMap -> ByteCodeOffset -> Maybe (ByteCodeIndex)
offsetIndex o i = IM.lookup (fromIntegral i) o

-- | Given an `OffsetMap` turn a offset into a bytecode index
evolveOffset ::
  EvolveM m
  => OffsetMap
  -> ByteCodeOffset
  -> m (ByteCodeIndex)
evolveOffset o i =
  case offsetIndex o i of
    Just a -> return $ a
    Nothing ->
      attributeError $ "Not valid offset " ++ show i

-- | Given low byte code we can create an `OffsetMap`
offsetMap :: ByteCode Low -> OffsetMap
offsetMap (ByteCode l v) =
  IM.fromList
    . ((fromIntegral l, V.length v):)
    . V.ifoldl' (\ls idx i -> (fromIntegral $ offset i, idx) : ls) []
    $ v

-- | Given an `OffsetMap` turn a offset into a bytecode index
devolveOffset ::
  DevolveM m
  => ByteCode Low
  -> ByteCodeIndex
  -> m (ByteCodeOffset)
devolveOffset v i = do
  case indexOffset v i of
    Just x ->
      return x
    Nothing ->
      error $ "Bad index " ++ show i

-- | Return the bytecode offset from the bytecode.
indexOffset :: ByteCode Low -> ByteCodeIndex -> Maybe (ByteCodeOffset)
indexOffset (ByteCode x bc) i =
  if i == V.length bc
    then return (fromIntegral x)
    else offset <$> bc V.!? i

devolveOffset' ::
  DevolveM m
  => V.Vector ByteCodeOffset
  -> ByteCodeIndex
  -> m (ByteCodeOffset)
devolveOffset' v i = do
  case indexOffset' v i of
    Just x ->
      return x
    Nothing ->
      error $ "Bad index " ++ show i

indexOffset' :: V.Vector ByteCodeOffset -> ByteCodeIndex -> Maybe (ByteCodeOffset)
indexOffset' c i = c V.!? i

-- | The byte code instruction is mostly used to succinctly read and
-- write an bytecode instruction from a bytestring.
data ByteCodeInst r = ByteCodeInst
  { offset :: !(ByteCodeOffset)
  , opcode :: !(ByteCodeOpr r)
  }

evolveByteCode :: EvolveM m => ByteCode Low -> m (OffsetMap, ByteCode High)
evolveByteCode bc@(ByteCode ln v) = do
  let !om = offsetMap bc
  x <- V.mapM (evolveByteCodeInst (evolveOffset om)) v
  return . (om,) $ ByteCode ln x

devolveByteCode :: DevolveM m => ByteCode High -> m (ByteCode Low)
devolveByteCode (ByteCode _ bc) = do
  -- Devolving byte code is not straight forward.
  (len, offsets) <- generateOffsets (V.map opcode bc)
  ByteCode (fromIntegral len)
    <$> V.mapM
      (devolveByteCodeInst (devolveOffset' offsets))
      (V.zipWith (flip ByteCodeInst . opcode) bc offsets)

generateOffsets :: DevolveM m => V.Vector (ByteCodeOpr High) -> m (Word16, V.Vector ByteCodeOffset)
generateOffsets bc = do
  (len, vect) <- V.foldM' acc (0,[]) bc
  return (len, V.fromList . reverse $ vect)
  where
    acc (off, lst) opr = do
      inst <- devolveByteCodeInst (const $ return 0) (ByteCodeInst off opr)
      let o = off + byteSize inst
      return (o, off:lst)

class ByteCodeStaged s where
  evolveBC ::
    EvolveM m
    => (ByteCodeOffset -> m ByteCodeIndex)
    -> s Low
    -> m (s High)

  devolveBC ::
    DevolveM m
    => (ByteCodeIndex -> m ByteCodeOffset)
    -> s High
    -> m (s Low)

byteSize :: ByteCodeInst Low -> Word16
byteSize inst =
  fromIntegral . BL.length . runPut $ putByteCode (offset inst) (opcode inst)

instance ByteCodeStaged ByteCodeInst where
  evolveBC = evolveByteCodeInst
  devolveBC = devolveByteCodeInst

evolveRefType :: EvolveM m => RefType Low -> m JRefType
evolveRefType = \case
  ArrayBaseType bt -> return $ JTArray (JTBase bt)
  Reference m _ -> link m

devolveRefType :: DevolveM m => JRefType -> m (RefType Low)
devolveRefType  = \case
  JTArray (JTBase bt) -> return $ ArrayBaseType bt
  c -> flip Reference (fromIntegral $ refTypeDepth c) <$> unlink c


evolveByteCodeInst ::
  EvolveM m
  => (ByteCodeOffset -> m ByteCodeIndex)
  -> ByteCodeInst Low
  -> m (ByteCodeInst High)
evolveByteCodeInst g (ByteCodeInst ofs opr) = do
  x <- case opr of
    Push c            -> label "Push" $ Push <$> evolveBConstant c
    Get fa r          -> label "Get" $ Get fa <$> link r
    Put fa r          -> label "Put" $ Put fa <$> link r
    Invoke r          -> label "Invoke" $ Invoke <$> evolve r
    New r             -> label "New" $ New <$> evolveRefType r
    CheckCast r       -> label "CheckCast" $ CheckCast <$> link r
    InstanceOf r      -> label "InstanceOf" $ InstanceOf <$> link r
    If cp on r        -> label "If" $ If cp on <$> calcOffset r
    IfRef b on r      -> label "IfRef" $ IfRef b on <$> calcOffset r
    Goto r            -> label "Goto" $ Goto <$> calcOffset r
    Jsr r             -> label "Jsr" $ Jsr <$> calcOffset r
    TableSwitch i (SwitchTable l ofss) -> label "TableSwitch" $
        TableSwitch <$> calcOffset i <*> (SwitchTable l <$> V.mapM calcOffset ofss)
    LookupSwitch i ofss -> label "LookupSwitch" $
        LookupSwitch <$> calcOffset i <*> V.mapM (\(a, b) -> (a,) <$> calcOffset b) ofss
    a                 -> return $ unsafeCoerce a
  return $ seq x (ByteCodeInst ofs x)
  where
    calcOffset r =
      g (fromIntegral $ fromIntegral ofs + r)


devolveByteCodeInst ::
  DevolveM m
  => (ByteCodeIndex -> m ByteCodeOffset)
  -> ByteCodeInst High
  -> m (ByteCodeInst Low)
devolveByteCodeInst g (ByteCodeInst ofs opr) =
  ByteCodeInst ofs <$> case opr of
    Push c            -> label "Push" $ Push <$> devolveBConstant c
    Get fa r          -> label "Get" $ Get fa <$> unlink r
    Put fa r          -> label "Put" $ Put fa <$> unlink r
    Invoke r          -> label "Invoke" $ Invoke <$> devolve r
    New r             -> label "New" $ New <$> devolveRefType r
    CheckCast r       -> label "CheckCast" $ CheckCast <$> unlink r
    InstanceOf r      -> label "InstanceOf" $ InstanceOf <$> unlink r
    If cp on r        -> label "If" $ If cp on <$> calcOffset r
    IfRef b on r      -> label "IfRef" $ IfRef b on <$> calcOffset r
    Goto r            -> label "Goto" $ Goto <$> calcOffset r
    Jsr r             -> label "Jsr" $ Jsr <$> calcOffset r
    TableSwitch i (SwitchTable l ofss) -> label "TableSwitch" $
        TableSwitch <$> calcOffset i <*> (SwitchTable l <$> V.mapM calcOffset ofss)
    LookupSwitch i ofss -> label "LookupSwitch" $
        LookupSwitch <$> calcOffset i <*> V.mapM (\(a, b) -> (a,) <$> calcOffset b) ofss
    a -> return $ unsafeCoerce a
  where
    calcOffset r = do
      x <- g r
      return (fromIntegral x - fromIntegral ofs)

instance Staged Invocation where
  evolve i =
    case i of
      InvkSpecial r     -> label "InvkSpecial" $ InvkSpecial <$> link r
      InvkVirtual r     -> label "InvkVirtual" $ InvkVirtual <$> link r
      InvkStatic r      -> label "InvkStatic" $ InvkStatic <$> link r
      InvkInterface w r -> label "InvkInterface" $ InvkInterface w <$> link r
      InvkDynamic r     -> label "InvkDynamic" $ InvkDynamic <$> link r

  devolve i =
    case i of
      InvkSpecial r     -> label "InvkSpecial" $ InvkSpecial <$> unlink r
      InvkVirtual r     -> label "InvkVirtual" $ InvkVirtual <$> unlink r
      InvkStatic r      -> label "InvkStatic" $ InvkStatic <$> unlink r
      InvkInterface w r -> label "InvkInterface" $ InvkInterface w <$> unlink r
      InvkDynamic r     -> label "InvkDynamic" $ InvkDynamic <$> unlink r


instance Binary (ByteCode Low) where
  get = do
    x <- getWord32be
    bs <- getLazyByteString (fromIntegral x)
    case runGetOrFail go bs of
      Right (_,_,bcs) -> return . ByteCode x . V.fromList $ bcs
      Left (_,_,msg)  -> fail msg
    where
      go = isEmpty >>= \t ->
        if t
          then return []
          else do
            x <- get
            (x:) <$> go

  put (ByteCode _ lst)= do
    let bs = runPut (mapM_ put lst)
    putWord32be (fromIntegral $ BL.length bs)
    putLazyByteString bs

instance Binary (ByteCodeInst Low) where
  get = do
    i <- (fromIntegral <$> bytesRead)
    x <- get
    return (ByteCodeInst i x)

  put x =
    putByteCode (offset x) $ opcode x

-- | A short relative bytecode ref is defined in correspondence with the
type ShortRelativeRef i = Choice Int16 ByteCodeIndex i

-- | A Long relative reference. The only reason this exist because
-- the signed nature of int, looses a bit.
type LongRelativeRef i = Choice Int32 ByteCodeIndex i

data ArithmeticType = MInt | MLong | MFloat | MDouble
  deriving (Show, Ord, Eq, Enum, Bounded, Generic, NFData)

data SmallArithmeticType = MByte | MChar | MShort
  deriving (Show, Ord, Eq, Enum, Bounded, Generic, NFData)

data RefType r
  = ArrayBaseType JBaseType
  | Reference (Ref JRefType r) Word8

data LocalType = LInt | LLong | LFloat | LDouble | LRef
  deriving (Show, Ord, Eq, Enum, Bounded, Generic, NFData)

data ArrayType
  = AByte | AChar | AShort | AInt | ALong
  | AFloat | ADouble | ARef
  deriving (Show, Eq, Ord, Generic, NFData)

data Invocation r
  = InvkSpecial !(DeepRef AbsMethodId r)
  -- ^ Variable since 52.0
  | InvkVirtual !(DeepRef AbsMethodId r)
  | InvkStatic !(DeepRef AbsMethodId r)
  -- ^ Variable since 52.0
  | InvkInterface !Word8 !(DeepRef AbsInterfaceMethodId r)
  -- ^ Should be a positive number
  | InvkDynamic !(DeepRef InvokeDynamic r)

data FieldAccess
  = FldStatic
  | FldField
  deriving (Show, Ord, Eq, Generic, NFData)

data OneOrTwo = One | Two
  deriving (Show, Ord, Bounded, Eq, Enum, Generic, NFData)

type WordSize = OneOrTwo

evolveBConstant :: EvolveM m => BConstant Low -> m (BConstant High)
evolveBConstant ccnst = do
  x <- evolve ccnst
  return $ case x of
    CNull    -> Nothing
    CIntM1   -> Just $ VInteger (-1)
    CInt0    -> Just $ VInteger 0
    CInt1    -> Just $ VInteger 1
    CInt2    -> Just $ VInteger 2
    CInt3    -> Just $ VInteger 3
    CInt4    -> Just $ VInteger 4
    CInt5    -> Just $ VInteger 5
    CLong0   -> Just $ VLong 0
    CLong1   -> Just $ VLong 1
    CFloat0  -> Just $ VFloat 0
    CFloat1  -> Just $ VFloat 1
    CFloat2  -> Just $ VFloat 2
    CDouble0 -> Just $ VDouble 0
    CDouble1 -> Just $ VDouble 1
    CByte i  -> Just $ VInteger (fromIntegral i)
    CShort i -> Just $ VInteger (fromIntegral i)
    CRef _ r -> Just $ r

devolveBConstant :: DevolveM m =>  BConstant High -> m (BConstant Low)
devolveBConstant x = do
  devolve v
  where
    v :: CConstant High
    v = case x of
      Nothing -> CNull
      Just x' -> case x' of
        VInteger i ->
          case i of
            (-1) -> CIntM1; 0 -> CInt0; 1 -> CInt1; 2 -> CInt2; 3 -> CInt3; 4 -> CInt4; 5 -> CInt5;
            i | -128 <= i && i <= 127      -> CByte (fromIntegral i)
              | -32768 <= i && i <= 32767 -> CShort (fromIntegral i)
              | otherwise -> CRef Nothing x'
        VLong 0                           -> CLong0
        VLong 1                           -> CLong1
        VFloat 0                          -> CFloat0
        VFloat 1                          -> CFloat1
        VFloat 2                          -> CFloat2
        VDouble 0                         -> CDouble0
        VDouble 1                         -> CDouble1
        VDouble _                         -> CRef (Just Two) x'
        VLong _                           -> CRef (Just Two) x'
        _                                 -> CRef Nothing x'


-- | A Wrapper around CConstant.
type BConstant r = Choice (CConstant r) (Maybe JValue) r

instance Staged CConstant where
  evolve x =
    case x of
      CRef w r -> label "Ref" $ CRef w <$> link r
      a        -> return $ unsafeCoerce a

  devolve x =
    case x of
      CRef w r -> label "Ref" $ CRef w <$> unlink r
      a        -> return $ unsafeCoerce a


data CConstant r
  = CNull

  | CIntM1
   -- ^ -1
  | CInt0
  | CInt1
  | CInt2
  | CInt3
  | CInt4
  | CInt5

  | CLong0
  | CLong1

  | CFloat0
  | CFloat1
  | CFloat2

  | CDouble0
  | CDouble1

  | CByte Int8
  | CShort Int16

  | CRef (Maybe WordSize) (Ref JValue r)

data BinOpr
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  deriving (Show, Ord, Eq, Generic, NFData)

data BitOpr
  = ShL
  | ShR
  | UShR
  | And
  | Or
  | XOr
  deriving (Show, Ord, Eq, Generic, NFData)

type LocalAddress = Word16
type IncrementAmount = Int16

maxWord8 :: Word16
maxWord8 = 0xff

data CmpOpr
  = CEq | CNe | CLt | CGe | CGt | CLe
  deriving (Show, Ord, Eq, Generic, NFData)

data CastOpr
  = CastDown SmallArithmeticType
  -- ^ Cast from Int to a smaller type
  | CastTo ArithmeticType ArithmeticType
  -- ^ Cast from any to any arithmetic type. Cannot be the same type.
  deriving (Show, Ord, Eq, Generic, NFData)


data SwitchTable r = SwitchTable
  { switchLow     :: Int32
  , switchOffsets :: V.Vector (LongRelativeRef r)
  }

switchHigh :: SwitchTable Low -> Int32
switchHigh st =
  len - 1 + switchLow st
  where
    len = fromIntegral . V.length $ switchOffsets st

data ByteCodeOpr r
  = ArrayLoad !ArrayType
  -- ^ aaload baload ...
  | ArrayStore !ArrayType
  -- ^ aastore bastore ...

  | Push !(BConstant r)

  | Load !LocalType !LocalAddress
  -- ^ aload_0, bload_2, iload 5 ...
  | Store !LocalType !LocalAddress
  -- ^ aload, bload ...

  | BinaryOpr !BinOpr !ArithmeticType
  -- ^ iadd ...
  | Neg !ArithmeticType
  -- ^ ineg ...

  | BitOpr !BitOpr !WordSize
  -- ^ Exclusively on int and long, identified by the word-size

  | IncrLocal !LocalAddress !IncrementAmount
  -- ^ Only works on ints, increment local #1, with #2

  | Cast !CastOpr
  -- ^ Only valid on different types

  | CompareLongs

  | CompareFloating !Bool !WordSize
  -- ^ Compare two floating values, #1 indicates if greater-than, #2
  -- is if float or double should be used.

  | If !CmpOpr !OneOrTwo !(ShortRelativeRef r)
  -- ^ compare with 0 if #2 is False, and two ints from the stack if
  -- True. the last value is the offset

  | IfRef !Bool !OneOrTwo !(ShortRelativeRef r)
  -- ^ check if two objects are equal, or not equal. If #2 is True, compare
  -- with null.

  | Goto !(LongRelativeRef r)
  | Jsr !(LongRelativeRef r)
  | Ret !LocalAddress

  | TableSwitch !(LongRelativeRef r) !(SwitchTable r)
  -- ^ a table switch has 2 values a `default` and a `SwitchTable`
  | LookupSwitch !(LongRelativeRef r) (V.Vector (Int32, (LongRelativeRef r)))
  -- ^ a lookup switch has a `default` value and a list of pairs.

  | Get !FieldAccess !(DeepRef (InClass FieldId) r)
  | Put !FieldAccess !(DeepRef (InClass FieldId) r)

  | Invoke !(Invocation r)

  | New !(Choice (RefType Low) JRefType r)

  | ArrayLength

  | Throw

  | CheckCast !(Ref ClassName r)
  | InstanceOf !(Ref ClassName r)

  | Monitor !Bool
  -- ^ True => Enter, False => Exit

  | Return !(Maybe LocalType)

  | Nop

  | Pop WordSize

  | Dup WordSize
  | DupX1 WordSize
  | DupX2 WordSize

  | Swap


instance Binary (ByteCodeOpr Low) where
  get = do
    cmd <- getWord8
    case cmd of
      0x00 -> return Nop
      0x01 -> return $ Push CNull

      0x02 -> return $ Push CIntM1
      0x03 -> return $ Push CInt0
      0x04 -> return $ Push CInt1
      0x05 -> return $ Push CInt2
      0x06 -> return $ Push CInt3
      0x07 -> return $ Push CInt4
      0x08 -> return $ Push CInt5

      0x09 -> return $ Push CLong0
      0x0a -> return $ Push CLong1

      0x0b -> return $ Push CFloat0
      0x0c -> return $ Push CFloat1
      0x0d -> return $ Push CFloat2

      0x0e -> return $ Push CDouble0
      0x0f -> return $ Push CDouble1

      0x10 -> Push . CByte <$> get
      0x11 -> Push . CShort <$> get

      0x12 -> Push . CRef Nothing . fromIntegral <$> getWord8
      0x13 -> Push . CRef (Just One) <$> get
      0x14 -> Push . CRef (Just Two) <$> get

      0x15 -> Load LInt . fromIntegral <$> getWord8
      0x16 -> Load LLong . fromIntegral <$> getWord8
      0x17 -> Load LFloat . fromIntegral <$> getWord8
      0x18 -> Load LDouble . fromIntegral <$> getWord8
      0x19 -> Load LRef . fromIntegral <$> getWord8

      0x1a -> return $ Load LInt 0
      0x1b -> return $ Load LInt 1
      0x1c -> return $ Load LInt 2
      0x1d -> return $ Load LInt 3

      0x1e -> return $ Load LLong 0
      0x1f -> return $ Load LLong 1
      0x20 -> return $ Load LLong 2
      0x21 -> return $ Load LLong 3

      0x22 -> return $ Load LFloat 0
      0x23 -> return $ Load LFloat 1
      0x24 -> return $ Load LFloat 2
      0x25 -> return $ Load LFloat 3

      0x26 -> return $ Load LDouble 0
      0x27 -> return $ Load LDouble 1
      0x28 -> return $ Load LDouble 2
      0x29 -> return $ Load LDouble 3

      0x2a -> return $ Load LRef 0
      0x2b -> return $ Load LRef 1
      0x2c -> return $ Load LRef 2
      0x2d -> return $ Load LRef 3

      0x2e -> return $ ArrayLoad AInt
      0x2f -> return $ ArrayLoad ALong
      0x30 -> return $ ArrayLoad AFloat
      0x31 -> return $ ArrayLoad ADouble
      0x32 -> return $ ArrayLoad ARef
      0x33 -> return $ ArrayLoad AByte
      0x34 -> return $ ArrayLoad AChar
      0x35 -> return $ ArrayLoad AShort

      0x36 -> Store LInt . fromIntegral <$> getWord8
      0x37 -> Store LLong . fromIntegral <$> getWord8
      0x38 -> Store LFloat . fromIntegral <$> getWord8
      0x39 -> Store LDouble . fromIntegral <$> getWord8
      0x3a -> Store LRef . fromIntegral <$> getWord8

      0x3b -> return $ Store LInt 0
      0x3c -> return $ Store LInt 1
      0x3d -> return $ Store LInt 2
      0x3e -> return $ Store LInt 3

      0x3f -> return $ Store LLong 0
      0x40 -> return $ Store LLong 1
      0x41 -> return $ Store LLong 2
      0x42 -> return $ Store LLong 3

      0x43 -> return $ Store LFloat 0
      0x44 -> return $ Store LFloat 1
      0x45 -> return $ Store LFloat 2
      0x46 -> return $ Store LFloat 3

      0x47 -> return $ Store LDouble 0
      0x48 -> return $ Store LDouble 1
      0x49 -> return $ Store LDouble 2
      0x4a -> return $ Store LDouble 3

      0x4b -> return $ Store LRef 0
      0x4c -> return $ Store LRef 1
      0x4d -> return $ Store LRef 2
      0x4e -> return $ Store LRef 3

      0x4f -> return $ ArrayStore AInt
      0x50 -> return $ ArrayStore ALong
      0x51 -> return $ ArrayStore AFloat
      0x52 -> return $ ArrayStore ADouble
      0x53 -> return $ ArrayStore ARef
      0x54 -> return $ ArrayStore AByte
      0x55 -> return $ ArrayStore AChar
      0x56 -> return $ ArrayStore AShort

      0x57 -> return $ Pop One
      0x58 -> return $ Pop Two

      0x59 -> return $ Dup One
      0x5a -> return $ DupX1 One
      0x5b -> return $ DupX2 One

      0x5c -> return $ Dup Two
      0x5d -> return $ DupX1 Two
      0x5e -> return $ DupX2 Two

      0x5f -> return $ Swap

      0x60 -> return $ BinaryOpr Add MInt
      0x61 -> return $ BinaryOpr Add MLong
      0x62 -> return $ BinaryOpr Add MFloat
      0x63 -> return $ BinaryOpr Add MDouble

      0x64 -> return $ BinaryOpr Sub MInt
      0x65 -> return $ BinaryOpr Sub MLong
      0x66 -> return $ BinaryOpr Sub MFloat
      0x67 -> return $ BinaryOpr Sub MDouble

      0x68 -> return $ BinaryOpr Mul MInt
      0x69 -> return $ BinaryOpr Mul MLong
      0x6a -> return $ BinaryOpr Mul MFloat
      0x6b -> return $ BinaryOpr Mul MDouble

      0x6c -> return $ BinaryOpr Div MInt
      0x6d -> return $ BinaryOpr Div MLong
      0x6e -> return $ BinaryOpr Div MFloat
      0x6f -> return $ BinaryOpr Div MDouble

      0x70 -> return $ BinaryOpr Rem MInt
      0x71 -> return $ BinaryOpr Rem MLong
      0x72 -> return $ BinaryOpr Rem MFloat
      0x73 -> return $ BinaryOpr Rem MDouble

      0x74 -> return $ Neg MInt
      0x75 -> return $ Neg MLong
      0x76 -> return $ Neg MFloat
      0x77 -> return $ Neg MDouble

      0x78 -> return $ BitOpr ShL One
      0x79 -> return $ BitOpr ShL Two
      0x7a -> return $ BitOpr ShR One
      0x7b -> return $ BitOpr ShR Two

      0x7c -> return $ BitOpr UShR One
      0x7d -> return $ BitOpr UShR Two

      0x7e -> return $ BitOpr And One
      0x7f -> return $ BitOpr And Two
      0x80 -> return $ BitOpr Or One
      0x81 -> return $ BitOpr Or Two
      0x82 -> return $ BitOpr XOr One
      0x83 -> return $ BitOpr XOr Two

      0x84 -> IncrLocal <$> (fromIntegral <$> getWord8) <*> (fromIntegral <$> getInt8)

      0x85 -> return $ Cast (CastTo MInt MLong)
      0x86 -> return $ Cast (CastTo MInt MFloat)
      0x87 -> return $ Cast (CastTo MInt MDouble)

      0x88 -> return $ Cast (CastTo MLong MInt)
      0x89 -> return $ Cast (CastTo MLong MFloat)
      0x8a -> return $ Cast (CastTo MLong MDouble)

      0x8b -> return $ Cast (CastTo MFloat MInt)
      0x8c -> return $ Cast (CastTo MFloat MLong)
      0x8d -> return $ Cast (CastTo MFloat MDouble)

      0x8e -> return $ Cast (CastTo MDouble MInt)
      0x8f -> return $ Cast (CastTo MDouble MLong)
      0x90 -> return $ Cast (CastTo MDouble MFloat)

      0x91 -> return $ Cast (CastDown MByte)
      0x92 -> return $ Cast (CastDown MChar)
      0x93 -> return $ Cast (CastDown MShort)

      0x94 -> return $ CompareLongs

      0x95 -> return $ CompareFloating True One
      0x96 -> return $ CompareFloating False One

      0x97 -> return $ CompareFloating True Two
      0x98 -> return $ CompareFloating False Two

      0x99 -> If CEq One <$> get
      0x9a -> If CNe One <$> get
      0x9b -> If CLt One <$> get
      0x9c -> If CGe One <$> get
      0x9d -> If CGt One <$> get
      0x9e -> If CLe One <$> get

      0x9f -> If CEq Two <$> get
      0xa0 -> If CNe Two <$> get
      0xa1 -> If CLt Two <$> get
      0xa2 -> If CGe Two <$> get
      0xa3 -> If CGt Two <$> get
      0xa4 -> If CLe Two <$> get

      0xa5 -> IfRef True Two <$> get
      0xa6 -> IfRef False Two <$> get

      0xa7 -> Goto . fromIntegral <$> getInt16be
      0xa8 -> Jsr . fromIntegral <$> getInt16be

      0xa9 -> Ret . fromIntegral <$> getWord8

      0xaa -> do
        offset' <- bytesRead
        let skipAmount = (4 - offset' `mod` 4) `mod` 4
        skip $ fromIntegral skipAmount
        dft <- getInt32be
        low <- getInt32be
        high <- getInt32be
        table <- V.replicateM (fromIntegral $ high - low + 1) getInt32be
        return $ TableSwitch dft (SwitchTable low table)

      0xab -> do
        offset' <- bytesRead
        let skipAmount = ((4 - offset' `mod` 4) `mod` 4)
        skip $ fromIntegral skipAmount
        dft <- getInt32be
        npairs <- getInt32be
        pairs <- V.replicateM (fromIntegral npairs) get
        return $ LookupSwitch dft pairs

      0xac -> return . Return . Just $ LInt
      0xad -> return . Return . Just $ LLong
      0xae -> return . Return . Just $ LFloat
      0xaf -> return . Return . Just $ LDouble
      0xb0 -> return . Return . Just $ LRef
      0xb1 -> return . Return $ Nothing

      0xb2 -> Get FldStatic <$> get
      0xb3 -> Put FldStatic <$> get

      0xb4 -> Get FldField <$> get
      0xb5 -> Put FldField <$> get

      0xb6 -> Invoke . InvkVirtual <$> get
      0xb7 -> Invoke . InvkSpecial <$> get
      0xb8 -> Invoke . InvkStatic <$> get
      0xb9 -> do
        ref <- get
        count <- get
        when (count == 0) $ fail "Should be not zero"
        zero <- getWord8
        when (zero /= 0) $ fail "Should be zero"
        return $ Invoke (InvkInterface count ref)
      0xba -> do
        ref <- get
        count <- getWord8
        when (count /= 0) $ fail "Should be zero"
        zero <- getWord8
        when (zero /= 0) $ fail "Should be zero"
        return $ Invoke (InvkDynamic ref)
      0xbb -> New . (flip Reference 0) <$> get

      0xbc -> do
        x <- getWord8
        New . ArrayBaseType <$> case x of
          4  -> return JTBoolean
          5  -> return JTChar
          6  -> return JTFloat
          7  -> return JTDouble
          8  -> return JTByte
          9  -> return JTShort
          10 -> return JTInt
          11 -> return JTLong
          _  -> fail $ "Unknown type '0x" ++ showHex x "'."

      0xbd -> New . (flip Reference 1) <$> get

      0xbe -> return ArrayLength

      0xbf -> return Throw

      0xc0 -> CheckCast <$> get
      0xc1 -> InstanceOf <$> get

      0xc2 -> return $ Monitor True
      0xc3 -> return $ Monitor False

      0xc4 -> do
        subopcode <- getWord8
        case subopcode of
          0x15 -> Load LInt <$> get
          0x16 -> Load LLong <$> get
          0x17 -> Load LFloat <$> get
          0x18 -> Load LDouble <$> get
          0x19 -> Load LRef <$> get

          0x36 -> Store LInt <$> get
          0x37 -> Store LLong <$> get
          0x38 -> Store LFloat <$> get
          0x39 -> Store LDouble <$> get
          0x3a -> Store LRef <$> get

          0x84 -> IncrLocal <$> get <*> get

          0xa9 -> Ret <$> get

          _ -> fail $ "Wide does not work for opcode '0x"
                ++ showHex subopcode "'"

      0xc5 -> New <$> (Reference <$> get <*> get)

      0xc6 -> IfRef False One <$> get
      0xc7 -> IfRef True One <$> get

      0xc8 -> Goto <$> getInt32be
      0xc9 -> Jsr <$> getInt32be

      _ -> fail $ "I do not know this bytecode '0x" ++ showHex cmd "'."

  {-# INLINABLE get #-}

  put = putByteCode 0

  {-# INLINE put #-}

putByteCode :: Word16 -> ByteCodeOpr Low -> Put
putByteCode n bc =
  case bc of
    Nop -> putWord8 0x00
    Push CNull -> putWord8 0x01

    Push CIntM1 -> putWord8 0x02
    Push CInt0 -> putWord8 0x03
    Push CInt1 -> putWord8 0x04
    Push CInt2 -> putWord8 0x05
    Push CInt3 -> putWord8 0x06
    Push CInt4 -> putWord8 0x07
    Push CInt5 -> putWord8 0x08

    Push CLong0 -> putWord8 0x09
    Push CLong1 -> putWord8 0x0a

    Push CFloat0 -> putWord8 0x0b
    Push CFloat1 -> putWord8 0x0c
    Push CFloat2 -> putWord8 0x0d

    Push CDouble0 -> putWord8 0x0e
    Push CDouble1 -> putWord8 0x0f

    Push (CByte x) -> putWord8 0x10 >> put x
    Push (CShort x) -> putWord8 0x11 >> put x

    Push (CRef (Just One) x) ->
      putWord8 0x13 >> put x
    -- In this case force the wide
    Push (CRef Nothing x)
      | x <= 0xff -> putWord8 0x12 >> (putWord8 . fromIntegral $ x)
      | otherwise -> putWord8 0x13 >> put x
    -- Here there is no direct restrictions
    Push (CRef (Just Two) r) -> putWord8 0x14 >> put r

    Load tp vl ->
      case tp of
        LInt ->
          case vl of
            0 -> putWord8 0x1a
            1 -> putWord8 0x1b
            2 -> putWord8 0x1c
            3 -> putWord8 0x1d
            a | a <= maxWord8 -> do
                putWord8 0x15
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x15 >> put a
        LLong ->
          case vl of
            0 -> putWord8 0x1e
            1 -> putWord8 0x1f
            2 -> putWord8 0x20
            3 -> putWord8 0x21
            a | a <= maxWord8 -> do
                putWord8 0x16
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x16 >> put a
        LFloat ->
          case vl of
            0 -> putWord8 0x22
            1 -> putWord8 0x23
            2 -> putWord8 0x24
            3 -> putWord8 0x25
            a | a <= maxWord8 -> do
                putWord8 0x17
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x17 >> put a
        LDouble ->
          case vl of
            0 -> putWord8 0x26
            1 -> putWord8 0x27
            2 -> putWord8 0x28
            3 -> putWord8 0x29
            a | a <= maxWord8 -> do
                putWord8 0x18
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x18 >> put a
        LRef ->
          case vl of
            0 -> putWord8 0x2a
            1 -> putWord8 0x2b
            2 -> putWord8 0x2c
            3 -> putWord8 0x2d
            a | a <= maxWord8 -> do
                putWord8 0x19
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x19 >> put a


    ArrayLoad t ->
      case t of
        AInt    -> putWord8 0x2e
        ALong   -> putWord8 0x2f
        AFloat  -> putWord8 0x30
        ADouble -> putWord8 0x31
        ARef    -> putWord8 0x32
        AByte   -> putWord8 0x33
        AChar   -> putWord8 0x34
        AShort  -> putWord8 0x35

    Store tp vl ->
      case tp of
        LInt ->
          case vl of
            0 -> putWord8 0x3b
            1 -> putWord8 0x3c
            2 -> putWord8 0x3d
            3 -> putWord8 0x3e
            a | a <= maxWord8 -> do
                putWord8 0x36
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x36 >> put a

        LLong ->
          case vl of
            0 -> putWord8 0x3f
            1 -> putWord8 0x40
            2 -> putWord8 0x41
            3 -> putWord8 0x42
            a | a <= maxWord8 -> do
                putWord8 0x37
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x37 >> put a

        LFloat ->
          case vl of
            0 -> putWord8 0x43
            1 -> putWord8 0x44
            2 -> putWord8 0x45
            3 -> putWord8 0x46
            a | a <= maxWord8 -> do
                putWord8 0x38
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x38 >> put a

        LDouble ->
          case vl of
            0 -> putWord8 0x47
            1 -> putWord8 0x48
            2 -> putWord8 0x49
            3 -> putWord8 0x4a
            a | a <= maxWord8 -> do
                putWord8 0x39
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x39 >> put a

        LRef ->
          case vl of
            0 -> putWord8 0x4b
            1 -> putWord8 0x4c
            2 -> putWord8 0x4d
            3 -> putWord8 0x4e
            a | a <= maxWord8 -> do
                putWord8 0x3a
                putWord8 (fromIntegral a)
            a -> do
              putWord8 0xc4 >> putWord8 0x3a >> put a

    ArrayStore AInt -> putWord8 0x4f
    ArrayStore ALong -> putWord8 0x50
    ArrayStore AFloat -> putWord8 0x51
    ArrayStore ADouble -> putWord8 0x52
    ArrayStore ARef -> putWord8 0x53
    ArrayStore AByte -> putWord8 0x54
    ArrayStore AChar -> putWord8 0x55
    ArrayStore AShort -> putWord8 0x56

    Pop One -> putWord8 0x57
    Pop Two -> putWord8 0x58

    Dup One -> putWord8 0x59
    DupX1 One -> putWord8 0x5a
    DupX2 One -> putWord8 0x5b

    Dup Two -> putWord8 0x5c
    DupX1 Two -> putWord8 0x5d
    DupX2 Two -> putWord8 0x5e

    Swap -> putWord8 0x5f

    BinaryOpr Add MInt -> putWord8 0x60
    BinaryOpr Add MLong -> putWord8 0x61
    BinaryOpr Add MFloat -> putWord8 0x62
    BinaryOpr Add MDouble -> putWord8 0x63

    BinaryOpr Sub MInt -> putWord8 0x64
    BinaryOpr Sub MLong -> putWord8 0x65
    BinaryOpr Sub MFloat -> putWord8 0x66
    BinaryOpr Sub MDouble -> putWord8 0x67

    BinaryOpr Mul MInt -> putWord8 0x68
    BinaryOpr Mul MLong -> putWord8 0x69
    BinaryOpr Mul MFloat -> putWord8 0x6a
    BinaryOpr Mul MDouble -> putWord8 0x6b

    BinaryOpr Div MInt -> putWord8 0x6c
    BinaryOpr Div MLong -> putWord8 0x6d
    BinaryOpr Div MFloat -> putWord8 0x6e
    BinaryOpr Div MDouble -> putWord8 0x6f

    BinaryOpr Rem MInt -> putWord8 0x70
    BinaryOpr Rem MLong -> putWord8 0x71
    BinaryOpr Rem MFloat -> putWord8 0x72
    BinaryOpr Rem MDouble -> putWord8 0x73

    Neg MInt -> putWord8 0x74
    Neg MLong -> putWord8 0x75
    Neg MFloat -> putWord8 0x76
    Neg MDouble -> putWord8 0x77

    BitOpr ShL One -> putWord8 0x78
    BitOpr ShL Two -> putWord8 0x79
    BitOpr ShR One -> putWord8 0x7a
    BitOpr ShR Two -> putWord8 0x7b

    BitOpr UShR One -> putWord8 0x7c
    BitOpr UShR Two -> putWord8 0x7d

    BitOpr And One -> putWord8 0x7e
    BitOpr And Two -> putWord8 0x7f
    BitOpr Or One -> putWord8 0x80
    BitOpr Or Two -> putWord8 0x81
    BitOpr XOr One -> putWord8 0x82
    BitOpr XOr Two -> putWord8 0x83

    IncrLocal s1 s2 ->
      if s1 > maxWord8 || s2 > fromIntegral (maxBound :: Int8) || s2 < fromIntegral (minBound :: Int8) then
        putWord8 0xc4 >> putWord8 0x84 >> put s1 >> put s2
      else
        putWord8 0x84 >> putWord8 (fromIntegral s1) >> putInt8 (fromIntegral s2)

    Cast a ->
      case a of
        CastTo MInt MLong -> putWord8 0x85
        CastTo MInt MFloat -> putWord8 0x86
        CastTo MInt MDouble -> putWord8 0x87

        CastTo MLong MInt -> putWord8 0x88
        CastTo MLong MFloat -> putWord8 0x89
        CastTo MLong MDouble -> putWord8 0x8a

        CastTo MFloat MInt -> putWord8 0x8b
        CastTo MFloat MLong -> putWord8 0x8c
        CastTo MFloat MDouble -> putWord8 0x8d

        CastTo MDouble MInt -> putWord8 0x8e
        CastTo MDouble MLong -> putWord8 0x8f
        CastTo MDouble MFloat -> putWord8 0x90

        CastDown MByte -> putWord8 0x91
        CastDown MChar -> putWord8 0x92
        CastDown MShort -> putWord8 0x93

        _ -> error $ "Cannot cast from " ++ show a ++ " to " ++ show a ++ "."

    CompareLongs -> putWord8 0x94

    CompareFloating True One -> putWord8 0x95
    CompareFloating False One -> putWord8 0x96

    CompareFloating True Two -> putWord8 0x97
    CompareFloating False Two -> putWord8 0x98

    If CEq One a -> putWord8 0x99 >> put a
    If CNe One a -> putWord8 0x9a >> put a
    If CLt One a -> putWord8 0x9b >> put a
    If CGe One a -> putWord8 0x9c >> put a
    If CGt One a -> putWord8 0x9d >> put a
    If CLe One a -> putWord8 0x9e >> put a

    If CEq Two a -> putWord8 0x9f >> put a
    If CNe Two a -> putWord8 0xa0 >> put a
    If CLt Two a -> putWord8 0xa1 >> put a
    If CGe Two a -> putWord8 0xa2 >> put a
    If CGt Two a -> putWord8 0xa3 >> put a
    If CLe Two a -> putWord8 0xa4 >> put a

    IfRef True Two a -> putWord8 0xa5 >> put a
    IfRef False Two a -> putWord8 0xa6 >> put a

    Goto a -> do
      if (abs a) < (2 :: Int32) ^ (15 :: Int32) then do
        putWord8 0xa7
        putInt16be (fromIntegral a)
      else do
        putWord8 0xc8
        putInt32be a
    Jsr a ->
      if (abs a) < (2 :: Int32) ^ (15 :: Int32) then do
        putWord8 0xa8
        putInt16be (fromIntegral a)
      else do
        putWord8 0xc9
        putInt32be a

    Ret a ->
      -- Check if correct size
      if a <= maxWord8
      then do
        putWord8 0xa9
        putWord8 (fromIntegral a)
      else do
        putWord8 0xc4 >> putWord8 0xa9 >> put a

    TableSwitch dft table -> do
      putWord8 0xaa
      -- missing pad
      replicateM_ (fromIntegral ((4 - (n + 1) `mod` 4) `mod` 4)) $ putWord8 0x00
      putInt32be dft
      putInt32be (switchLow table)
      putInt32be (switchHigh table)
      V.mapM_ putInt32be (switchOffsets table)

    LookupSwitch dft pairs -> do
      putWord8 0xab
      replicateM_ (fromIntegral ((4 - (n + 1) `mod` 4) `mod` 4)) $ putWord8 0x00
      putInt32be dft
      putInt32be . fromIntegral $ V.length pairs
      V.mapM_ put pairs

    Return ( Just LInt ) -> putWord8 0xac
    Return ( Just LLong ) -> putWord8 0xad
    Return ( Just LFloat ) -> putWord8 0xae
    Return ( Just LDouble ) -> putWord8 0xaf
    Return ( Just LRef ) -> putWord8 0xb0
    Return Nothing -> putWord8 0xb1

    Get FldStatic a -> putWord8 0xb2 >> put a
    Put FldStatic a -> putWord8 0xb3 >> put a

    Get FldField a -> putWord8 0xb4 >> put a
    Put FldField a -> putWord8 0xb5 >> put a

    Invoke i ->
      case i of
        InvkVirtual a -> putWord8 0xb6 >> put a
        InvkSpecial a -> putWord8 0xb7 >> put a
        InvkStatic a -> putWord8 0xb8 >> put a
        InvkInterface count a -> do
          when (count == 0) $ error "Should be not zero"
          putWord8 0xb9
          put a
          put count
          putWord8 0
        InvkDynamic a ->
          putWord8 0xba >> put a >> putWord8 0 >> putWord8 0

    New a ->
      case a of
        ArrayBaseType bt -> case bt of
          JTBoolean -> putWord8 0xbc >> putWord8 4
          JTChar    -> putWord8 0xbc >> putWord8 5
          JTFloat   -> putWord8 0xbc >> putWord8 6
          JTDouble  -> putWord8 0xbc >> putWord8 7
          JTByte    -> putWord8 0xbc >> putWord8 8
          JTShort   -> putWord8 0xbc >> putWord8 9
          JTInt     -> putWord8 0xbc >> putWord8 10
          JTLong    -> putWord8 0xbc >> putWord8 11
        Reference p 0 -> putWord8 0xbb >> put p
        Reference p 1 -> putWord8 0xbd >> put p
        Reference p n -> putWord8 0xc5 >> put p >> put n

    ArrayLength -> putWord8 0xbe
    Throw -> putWord8 0xbf

    CheckCast a -> putWord8 0xc0 >> put a
    InstanceOf a -> putWord8 0xc1 >> put a

    Monitor True -> putWord8 0xc2
    Monitor False -> putWord8 0xc3


    IfRef False One a -> putWord8 0xc6 >> put a
    IfRef True One a -> putWord8 0xc7 >> put a

instance Eq (ByteCode High) where
  ByteCode _ a == ByteCode _ b =
    a == b

instance Eq (ByteCode Low) where
  ByteCode i a == ByteCode j b =
    i == j && a == b

deriving instance Ord (ByteCode Low)

instance Eq (ByteCodeInst High) where
  ByteCodeInst _ a == ByteCodeInst _ b =
    a == b

instance Eq (ByteCodeInst Low) where
  ByteCodeInst i a == ByteCodeInst j b =
    i == j && a == b

deriving instance Ord (ByteCodeInst Low)

$(deriveThese ''ByteCode [''Show, ''Generic, ''NFData])
$(deriveThese ''ByteCodeInst [''Show, ''Generic, ''NFData])
$(deriveBase ''ByteCodeOpr)
$(deriveBase ''SwitchTable)
$(deriveBase ''Invocation)
$(deriveBase ''RefType)
$(deriveBase ''CConstant)
