{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.Attribute.CodeTest where

-- -- import           Data.Int
import qualified Data.Vector as V
import           Generic.Random
import           SpecHelper

import           Language.JVM.AttributeTest  ()
import           Language.JVM.Attribute.Code
import           Language.JVM.Constant
import           Language.JVM.ConstantTest
import           Language.JVM.UtilsTest      ()
import           Language.JVM.Utils


-- prop_encode_and_decode_ByteCode :: ByteCode -> Property
-- prop_encode_and_decode_ByteCode = isoBinary

-- prop_encode_and_decode :: Code Low -> Property
-- prop_encode_and_decode = isoBinary

-- prop_encode_and_decode_ByteCode :: ByteCode Low -> Property
-- prop_encode_and_decode_ByteCode = isoBinary

-- prop_encode_and_decode_ByteCodeOpr :: ByteCodeOpr Low -> Property
-- prop_encode_and_decode_ByteCodeOpr = isoBinary

-- instance Arbitrary (Code Low) where
--   arbitrary = Code
--     <$> arbitrary
--     <*> arbitrary
--     <*> arbitrary
--     <*> arbitrary
--     <*> arbitrary
--   shrink (Code ms ml bc _ _) =
--     [ Code ms ml bc' (SizedList []) (SizedList []) | bc' <- shrink bc ]

-- instance Arbitrary (ByteCode Low) where
--   arbitrary =
--      ByteCode . calculateOffsets <$> arbitrary

--   shrink (ByteCode xs) =
--     ByteCode . calculateOffsets <$> shrink (map opcode xs)

-- instance Arbitrary ArithmeticType where
--   arbitrary = elements [ MInt, MLong, MFloat, MDouble ]

-- instance Arbitrary LocalType where
--   arbitrary = elements [ LInt, LLong, LFloat, LDouble, LRef ]

-- -- instance Arbitrary (ByteCodeOpr Low) where
-- --   arbitrary =
-- --    genericArbitrary uniform

-- instance Arbitrary a => Arbitrary (V.Vector a) where
--   arbitrary = V.fromList <$> arbitrary

-- instance Arbitrary ArrayType where
--   arbitrary = genericArbitrary uniform

-- instance Arbitrary (ExactArrayType Low) where
--   arbitrary =
--     oneof
--     [ pure EABoolean
--     , pure EAByte
--     , pure EAChar
--     , pure EAShort
--     , pure EAInt
--     , pure EALong
--     , pure EAFloat
--     , pure EADouble
--     , EARef <$> arbitrary
--     ]

-- instance Arbitrary BinOpr where
--   arbitrary = genericArbitrary uniform

-- instance Arbitrary CastOpr where
--   arbitrary =
--     oneof . map pure $
--       [ CastTo MInt MLong
--       , CastTo MInt MFloat
--       , CastTo MInt MDouble

--       , CastTo MLong MInt
--       , CastTo MLong MFloat
--       , CastTo MLong MDouble

--       , CastTo MFloat MInt
--       , CastTo MFloat MLong
--       , CastTo MFloat MDouble

--       , CastTo MDouble MInt
--       , CastTo MDouble MLong
--       , CastTo MDouble MFloat

--       , CastDown MByte
--       , CastDown MChar
--       , CastDown MShort
--       ]

-- instance Arbitrary BitOpr where
--   arbitrary = genericArbitrary uniform

-- instance Arbitrary OneOrTwo where
--   arbitrary = genericArbitrary uniform

-- instance Arbitrary SmallArithmeticType where
--   arbitrary = genericArbitrary uniform

-- instance Arbitrary CmpOpr where
--   arbitrary = genericArbitrary uniform

-- instance Arbitrary FieldAccess where
--   arbitrary = genericArbitrary uniform

-- instance Arbitrary Invokation where
--   arbitrary =
--     oneof
--     [ pure InvkSpecial
--     , pure InvkVirtual
--     , pure InvkStatic
--     , InvkInterface <$> arbitrary `suchThat` \i -> i > 0
--     , pure InvkDynamic
--     ]

-- -- instance Arbitrary (CConstant Low) where
-- --   arbitrary = genericArbitrary uniform

-- instance Arbitrary SwitchTable where
--   arbitrary = genericArbitrary uniform

-- instance Arbitrary (ExceptionTable Low) where
--   arbitrary = ExceptionTable
--     <$> arbitrary
--     <*> arbitrary
--     <*> arbitrary
--     <*> arbitrary
