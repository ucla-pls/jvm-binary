{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JVM.Attribute.InnerClassesSpec where

import           SpecHelper
-- import qualified Data.Set as S

import           Language.JVM.ConstantSpec               ()

import           Language.JVM.Attribute.InnerClasses
import           Language.JVM

spec :: Spec
spec = do
  it "can do a roundtrip" $ property $ prop_roundtrip_InnerClassesSpec
  --spec_inner_classes

prop_roundtrip_InnerClassesSpec :: InnerClasses High -> Property
prop_roundtrip_InnerClassesSpec = isoRoundtrip

-- spec_inner_classes ::  SpecWith ()
-- spec_inner_classes = do
--   it "can find the inner classes of 'Streams$5'" $ do
--     tc <- withTestClass "Streams$5"
--     cInnerClasses tc `shouldBe`
--       [
--           InnerClass {
--               icClassName = "java/util/PrimitiveIterator$OfDouble",
--               icOuterClassName = Just "java/util/PrimitiveIterator",
--               icInnerName = Just "OfDouble",
--               icInnerAccessFlags = BitSet $ S.fromList [ICPublic, ICStatic, ICInterface, ICAbstract]
--           },
--           InnerClass {
--               icClassName = "com/google/common/collect/Streams$DoubleFunctionWithIndex",
--               icOuterClassName = Just "com/google/common/collect/Streams",
--               icInnerName = Just "DoubleFunctionWithIndex",
--               icInnerAccessFlags = BitSet $ S.fromList [ICPublic, ICStatic, ICInterface, ICAbstract]
--           },
--           InnerClass {
--               icClassName = "com/google/common/collect/Streams$5",
--               icOuterClassName = Nothing,
--               icInnerName = Nothing,
--               icInnerAccessFlags = BitSet $ S.fromList [ ICStatic ]
--           },
--           InnerClass {
--               icClassName = "java/util/Spliterators$AbstractSpliterator",
--               icOuterClassName = Just "java/util/Spliterators",
--               icInnerName = Just "AbstractSpliterator",
--               icInnerAccessFlags = BitSet $ S.fromList [ICPublic, ICStatic, ICAbstract]
--           }
--       ]

-- test_real_signatures :: SpecWith ()
-- test_real_signatures = do
--   it "can handle Iterator" $ do
--    let signature = "<T:Ljava/lang/Object;>Ljava/lang/Object;"

instance Arbitrary (InnerClasses High) where
  arbitrary = genericArbitraryU

instance Arbitrary (InnerClass High) where
  arbitrary = genericArbitraryU


  -- "Lcom/apple/eawt/_AppEventHandler$_AppEventDispatcher<Lcom/apple/eawt/QuitHandler;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/ScreenSleepListener;Lcom/apple/eawt/AppEvent$ScreenSleepEvent;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/SystemSleepListener;Lcom/apple/eawt/AppEvent$SystemSleepEvent;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/UserSessionListener;Lcom/apple/eawt/AppEvent$UserSessionEvent;>;"
