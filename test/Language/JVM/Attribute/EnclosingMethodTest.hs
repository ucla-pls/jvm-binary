{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JVM.Attribute.EnclosingMethodTest where

import           SpecHelper

import           Language.JVM.ConstantTest               ()

import           Language.JVM.Attribute.EnclosingMethod
import           Language.JVM

prop_roundtrip_EnclosingMethodTest :: EnclosingMethod High -> Property
prop_roundtrip_EnclosingMethodTest = isoRoundtrip


spec_enclosing ::  SpecWith ()
spec_enclosing = do
  it "can find an enclosing method in 'Streams$5'" $ do
    tc <- withTestClass "Streams$5"
    cEnclosingMethod tc `shouldBe`
       Just (EnclosingMethod
             "com/google/common/collect/Streams"
             (Just ("mapWithIndex:(Ljava/util/stream/DoubleStream;Lcom/google/common/collect/Streams$DoubleFunctionWithIndex;)Ljava/util/stream/Stream;"))
            )

-- test_real_signatures :: SpecWith ()
-- test_real_signatures = do
--   it "can handle Iterator" $ do
--    let signature = "<T:Ljava/lang/Object;>Ljava/lang/Object;"

instance Arbitrary (EnclosingMethod High) where
  arbitrary = genericArbitraryU


  -- "Lcom/apple/eawt/_AppEventHandler$_AppEventDispatcher<Lcom/apple/eawt/QuitHandler;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/ScreenSleepListener;Lcom/apple/eawt/AppEvent$ScreenSleepEvent;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/SystemSleepListener;Lcom/apple/eawt/AppEvent$SystemSleepEvent;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/UserSessionListener;Lcom/apple/eawt/AppEvent$UserSessionEvent;>;"
