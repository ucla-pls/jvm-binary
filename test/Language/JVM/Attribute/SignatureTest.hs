{-# LANGUAGE FlexibleInstances #-}

module Language.JVM.Attribute.SignatureTest where

import           SpecHelper

import           Language.JVM.ConstantTest               ()

import           Language.JVM.Attribute.Signature
import           Language.JVM

prop_roundtrip_SignatureTest :: Signature High -> Property
prop_roundtrip_SignatureTest = isoRoundtrip

-- test_real_signatures :: SpecWith ()
-- test_real_signatures = do
--   it "can handle Iterator" $ do
--    let signature = "<T:Ljava/lang/Object;>Ljava/lang/Object;"

instance Arbitrary (Signature High) where
  arbitrary = genericArbitraryU


-- "Lcom/apple/eawt/_AppEventHandler$_AppEventDispatcher<Lcom/apple/eawt/QuitHandler;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/ScreenSleepListener;Lcom/apple/eawt/AppEvent$ScreenSleepEvent;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/SystemSleepListener;Lcom/apple/eawt/AppEvent$SystemSleepEvent;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/UserSessionListener;Lcom/apple/eawt/AppEvent$UserSessionEvent;>;"
