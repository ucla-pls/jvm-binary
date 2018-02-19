{-|
Module      : Language.JVM.Attribute.Code
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu
-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Language.JVM.Attribute.Code
  ( Code (..)

  , ByteCode (..)
  , ExceptionTable (..)

  , ByteCodeInst (..)
  , ByteCodeOpr (..)
  , calculateOffsets

  , CConstant (..)
  , OneOrTwo (..)

  , SwitchTable (..)
  , switchHigh

  , FieldAccess (..)
  , Invokation (..)

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
  , ExactArrayType (..)

  -- * Renames 
  , WordSize
  , LongOffset
  , Offset
  , LocalAddress 
  , IncrementAmount 
  ) where

import           GHC.Generics                (Generic)

import           Prelude                     hiding (fail)
-- import qualified Prelude                     as P

import           Numeric                     (showHex)

import           Control.DeepSeq             (NFData)
import           Control.Monad               hiding (fail)
import           Control.Monad.Fail          (fail)

import           Data.Binary
import           Data.Binary.Get             hiding (Get)
import           Data.Binary.Put             hiding (Put)
import qualified Data.ByteString.Lazy        as BL
import           Data.Int
import qualified Data.Vector                 as V

import           Language.JVM.Attribute.Base
import           Language.JVM.ConstantPool
import           Language.JVM.Utils

-- | Code contains the actual byte-code. The 'i' type parameter is added to
-- allow indicate the two stages of the code file, before and after access to
-- the 'ConstantPool'. i should be either 'Ref' or 'Deref'.
data Code r = Code
  { codeMaxStack       :: Word16
  , codeMaxLocals      :: Word16
  , codeByteCode       :: (ByteCode r)
  , codeExceptionTable :: SizedList16 (ExceptionTable r)
  , codeAttributes     :: SizedList16 (Attribute r)
  }

newtype ByteCode i = ByteCode
  { unByteCode :: [ByteCodeInst i]
  }

instance Binary (ByteCode Index) where
  get = do
    x <- getWord32be
    bs <- getLazyByteString (fromIntegral x)
    case runGetOrFail go bs of
      Right (_,_,bcs) -> return $ ByteCode bcs
      Left (_,_,msg)  -> fail msg
    where
      go = isEmpty >>= \t ->
        if t
          then return []
          else do
            x <- get
            (x:) <$> go

  put (ByteCode lst)= do
    let bs = runPut (mapM_ put lst)
    putWord32be (fromIntegral $ BL.length bs)
    putLazyByteString bs


data ExceptionTable i = ExceptionTable
  { start     :: ! Word16
  -- ^ Inclusive program counter into 'code'
  , end       :: ! Word16
  -- ^ Exclusive program counter into 'code'
  , handler   :: ! Word16
  -- ^ A program counter into 'code' indicating the handler.
  , catchType :: ! (Ref i ClassName)
  }

data ByteCodeInst i = ByteCodeInst
  { offset :: LongOffset
  , opcode :: ByteCodeOpr i
  }

calculateOffsets :: [ByteCodeOpr Index] -> [ByteCodeInst Index]
calculateOffsets = go 0
  where
    go n (bc:rest) =
      ByteCodeInst n bc : go (byteSize n bc + n) rest
    go _ [] = []

instance Binary (ByteCodeInst Index) where
  get =
    ByteCodeInst <$> (fromIntegral <$> bytesRead) <*> get
  put x =
    putByteCode (offset x) $ opcode x

data ArithmeticType = MInt | MLong | MFloat | MDouble
  deriving (Show, Eq, Enum, Bounded, Generic, NFData)

data SmallArithmeticType = MByte | MChar | MShort
  deriving (Show, Eq, Enum, Bounded, Generic, NFData)

data LocalType = LInt | LLong | LFloat | LDouble | LRef
  deriving (Show, Eq, Enum, Bounded, Generic, NFData)

data ArrayType
  = AByte | AChar | AShort | AInt | ALong
  | AFloat | ADouble | ARef
  deriving (Show, Eq, Generic, NFData)

data ExactArrayType r
  = EABoolean | EAByte | EAChar | EAShort | EAInt | EALong
  | EAFloat | EADouble | EARef (Ref r ClassName)
  deriving (Show, Eq, Generic, NFData)

data Invokation
  = InvkSpecial
  | InvkVirtual
  | InvkStatic
  | InvkInterface Word8
  -- ^ Should be a positive number
  | InvkDynamic
  deriving (Show, Eq, Generic, NFData)

data FieldAccess
  = FldStatic
  | FldField
  deriving (Show, Eq, Generic, NFData)

data OneOrTwo = One | Two
  deriving (Show, Ord, Bounded, Eq, Enum, Generic, NFData)

type WordSize = OneOrTwo

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

  | CHalfRef (Ref r (Constant r))
  | CRef WordSize (Ref r (Constant r))

data BinOpr
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  deriving (Show, Eq, Generic, NFData)

data BitOpr
  = ShL
  | ShR
  | UShR
  | And
  | Or
  | XOr
  deriving (Show, Eq, Generic, NFData)

type Offset = Int16
type LongOffset = Int32
type LocalAddress = Word16
type IncrementAmount = Int16

maxWord8 :: Word16
maxWord8 = 0xff

maxInt8 :: Int16
maxInt8 = 0x7f

data CmpOpr
  = CEq | CNe | CLt | CGe | CGt | CLe
  deriving (Show, Eq, Generic, NFData)

data CastOpr
  = CastDown SmallArithmeticType
  -- ^ Cast from Int to a smaller type
  | CastTo ArithmeticType ArithmeticType
  -- ^ Cast from any to any arithmetic type. Cannot be the same type.
  deriving (Show, Eq, Generic, NFData)


data SwitchTable = SwitchTable
  { switchLow :: Int32
  , switchOffsets :: V.Vector (LongOffset)
  } deriving (Show, Eq, Generic, NFData)

switchHigh :: SwitchTable -> Int32
switchHigh st =
  len - 1 + switchLow st
  where
    len = fromIntegral . V.length $ switchOffsets st

data ByteCodeOpr r
  = ArrayLoad ArrayType
  -- ^ aaload baload ...
  | ArrayStore ArrayType
  -- ^ aastore bastore ...

  | Push (CConstant r)

  | Load LocalType LocalAddress
  -- ^ aload_0, bload_2, iload 5 ...
  | Store LocalType LocalAddress
  -- ^ aload, bload ...

  | BinaryOpr BinOpr ArithmeticType
  -- ^ iadd ...
  | Neg ArithmeticType
  -- ^ ineg ...

  | BitOpr BitOpr WordSize
  -- ^ Exclusively on int and long, identified by the word-size

  | IncrLocal !LocalAddress !IncrementAmount
  -- ^ Only works on ints, increment local #1, with #2

  | Cast CastOpr
  -- ^ Only valid on different types

  | CompareLongs

  | CompareFloating Bool WordSize
  -- ^ Compare two floating values, #1 indicates if greater-than, #2
  -- is if float or double should be used.

  | If CmpOpr OneOrTwo Offset
  -- ^ compare with 0 if #2 is False, and two ints from the stack if
  -- True. the last value is the offset

  | IfRef Bool OneOrTwo Offset
  -- ^ check if two objects are equal, or not equal. If #2 is True, compare
  -- with null.

  | Goto LongOffset
  | Jsr LongOffset
  | Ret LocalAddress

  | TableSwitch Int32 SwitchTable
  -- ^ a table switch has 2 values a `default` and a `SwitchTable`
  | LookupSwitch Int32 (V.Vector (Int32, Int32))
  -- ^ a lookup switch has a `default` value and a list of pairs.

  | Get FieldAccess (Ref r (InClass FieldId r))
  | Put FieldAccess (Ref r (InClass FieldId r))

  | Invoke Invokation (Ref r (InClass MethodId r))

  | New (Ref r ClassName)

  | NewArray (ExactArrayType r)

  | ArrayLength

  | Throw

  | CheckCast (Ref r ClassName)
  | InstanceOf (Ref r ClassName)

  | Monitor Bool
  -- ^ True => Enter, False => Exit

  -- TODO: Fix this so that its more clear what it points to.
  | MultiNewArray (Ref r ClassName) Word8
  -- ^ Create a new multi array of #1 and with #2 dimensions
  -- ^ This might point to an array type.

  | Return (Maybe LocalType)

  | Nop

  | Pop WordSize

  | Dup WordSize
  | DupX1 WordSize
  | DupX2 WordSize

  | Swap

-- deriving (Eq, Generic, NFData)

byteSize :: LongOffset -> ByteCodeOpr Index -> LongOffset
byteSize n x =
  fromIntegral . BL.length . runPut $ putByteCode n x
  
instance Binary (ByteCodeOpr Index) where
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

      0x12 -> Push . CHalfRef <$> get
      0x13 -> Push . CRef One <$> get
      0x14 -> Push . CRef Two <$> get

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

      0xb6 -> Invoke InvkVirtual <$> get
      0xb7 -> Invoke InvkSpecial <$> get
      0xb8 -> Invoke InvkStatic <$> get
      0xb9 -> do
        ref <- get
        count <- get
        when (count == 0) $ fail "Should be not zero"
        zero <- getWord8
        when (zero /= 0) $ fail "Should be zero"
        return $ Invoke (InvkInterface count) ref
      0xba -> do
        ref <- get
        count <- getWord8
        when (count /= 0) $ fail "Should be zero"
        zero <- getWord8
        when (zero /= 0) $ fail "Should be zero"
        return $ Invoke InvkDynamic ref
      0xbb -> New <$> get

      0xbc -> do
        x <- getWord8
        NewArray <$> case x of
          4  -> return EABoolean
          5  -> return EAChar
          6  -> return EAFloat
          7  -> return EADouble
          8  -> return EAByte
          9  -> return EAShort
          10 -> return EAInt
          11 -> return EALong
          _  -> fail $ "Unknown type '0x" ++ showHex x "'."

      0xbd -> NewArray . EARef <$> get

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

      0xc5 -> MultiNewArray <$> get <*> get

      0xc6 -> IfRef False One <$> get
      0xc7 -> IfRef True One <$> get

      0xc8 -> Goto <$> getInt32be
      0xc9 -> Jsr <$> getInt32be

      _ -> fail $ "I do not know this bytecode '0x" ++ showHex cmd "'."

  put = putByteCode 0

putByteCode :: LongOffset -> ByteCodeOpr Index -> Put
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

    Push (CHalfRef x) -> putWord8 0x12 >> put x
    Push (CRef One r) -> putWord8 0x13 >> put r
    Push (CRef Two r) -> putWord8 0x14 >> put r

    -- 0x15 -> Load LInt . fromIntegral <$> getWord8
    -- 0x16 -> Load LLong . fromIntegral <$> getWord8
    -- 0x17 -> Load LFloat . fromIntegral <$> getWord8
    -- 0x18 -> Load LDouble . fromIntegral <$> getWord8
    -- 0x19 -> Load LRef . fromIntegral <$> getWord8

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
        AInt -> putWord8 0x2e
        ALong -> putWord8 0x2f
        AFloat -> putWord8 0x30
        ADouble -> putWord8 0x31
        ARef -> putWord8 0x32
        AByte -> putWord8 0x33
        AChar -> putWord8 0x34
        AShort -> putWord8 0x35

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
      if s1 > maxWord8 || abs s2 > maxInt8 then
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

    -- 0xab -> do
    --   offset' <- bytesRead
    --   let skipAmount = (4 - offset' `mod` 4) `mod` 4
    --   if skipAmount > 0 then skip $ fromIntegral skipAmount else return ()
    --   dft <- getInt32be
    --   npairs <- getInt32be
    --   pairs <- V.replicateM (fromIntegral npairs) get
    --   return $ LookupSwitch dft pairs

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

    Invoke InvkVirtual a -> putWord8 0xb6 >> put a
    Invoke InvkSpecial a -> putWord8 0xb7 >> put a
    Invoke InvkStatic a -> putWord8 0xb8 >> put a
    Invoke (InvkInterface count) ref -> do
      when (count == 0) $ error "Should be not zero"
      putWord8 0xb9
      put ref
      put count
      putWord8 0
    Invoke InvkDynamic ref -> putWord8 0xba >> put ref >> putWord8 0 >> putWord8 0

    -- 0xb9 -> do
    --   ref <- get
    --   count <- get
    --   when (count == 0) $ fail "Should be not zero"
    --   zero <- getWord8
    --   when (zero /= 0) $ fail "Should be zero"
    --   return $ Invoke (InvkInterface count) ref
    -- 0xba -> do
    --   ref <- get
    --   count <- getWord8
    --   when (count /= 0) $ fail "Should be zero"
    --   zero <- getWord8
    --   when (zero /= 0) $ fail "Should be zero"
    --   return $ Invoke InvkDynamic ref


    New a -> putWord8 0xbb >> put a
    NewArray x -> do
      case x of
        EABoolean -> putWord8 0xbc >> putWord8 4
        EAChar    -> putWord8 0xbc >> putWord8 5
        EAFloat   -> putWord8 0xbc >> putWord8 6
        EADouble  -> putWord8 0xbc >> putWord8 7
        EAByte    -> putWord8 0xbc >> putWord8 8
        EAShort   -> putWord8 0xbc >> putWord8 9
        EAInt     -> putWord8 0xbc >> putWord8 10
        EALong    -> putWord8 0xbc >> putWord8 11
        EARef a   -> putWord8 0xbd >> put a
    ArrayLength -> putWord8 0xbe
    Throw -> putWord8 0xbf

    CheckCast a -> putWord8 0xc0 >> put a
    InstanceOf a -> putWord8 0xc1 >> put a

    Monitor True -> putWord8 0xc2
    Monitor False -> putWord8 0xc3


    MultiNewArray a b -> putWord8 0xc5 >> put a >> put b

    IfRef False One a -> putWord8 0xc6 >> put a
    IfRef True One a -> putWord8 0xc7 >> put a


$(deriveBaseB ''Index ''Code)
$(deriveBase ''ByteCode)
$(deriveBaseB ''Index ''ExceptionTable)
$(deriveBase ''ByteCodeInst)
$(deriveBase ''CConstant)
$(deriveBase ''ByteCodeOpr)
