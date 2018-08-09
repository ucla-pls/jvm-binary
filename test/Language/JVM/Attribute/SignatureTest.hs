{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JVM.Attribute.SignatureTest where

import           SpecHelper

import           Language.JVM.ConstantTest               ()

import           Language.JVM.Attribute.Signature
import           Language.JVM

import qualified Data.Text as Text
import Data.Attoparsec.Text

import Debug.Trace

prop_roundtrip_SignatureTest :: Signature High -> Property
prop_roundtrip_SignatureTest = isoRoundtrip

prop_field_signature :: FieldSignature -> Property
prop_field_signature sig =
  let txt = fieldSignatureToText sig in
    counterexample (Text.unpack txt) $
      fieldSignatureFromText txt === Right sig

prop_method_signature :: MethodSignature -> Property
prop_method_signature sig =
  let txt = methodSignatureToText sig in
    counterexample (Text.unpack txt) $
      methodSignatureFromText txt === Right sig

prop_class_signature :: ClassSignature -> Property
prop_class_signature sig =
  let txt = classSignatureToText sig in
    counterexample (Text.unpack txt) $
      classSignatureFromText txt === Right sig

spec_real_signatures :: SpecWith ()
spec_real_signatures = do
  it "can handle this class type Ljava/lang/Object;" $ do
   let sig = "Ljava/lang/Object;"
   parseOnly referenceTypeP sig `shouldBe`
       Right (RefClassType (ClassType "java/lang/Object" []))

  it "can handle this type parameter E:Ljava/lang/Object;" $ do
   let sig = "E:Ljava/lang/Object;"
   parseOnly typeParameterP sig `shouldBe`
       Right (TypeParameter {
              tpIndentifier = "E",
              tpClassBound = Just (RefClassType (ClassType "java/lang/Object" [])),
              tpInterfaceBound = []
              })
  it "can handle this type parameter <E:Ljava/lang/Object;>" $ do
   let sig = "<E:Ljava/lang/Object;>"
   parseOnly typeParametersP sig `shouldBe`
       Right [
           TypeParameter {
               tpIndentifier = "E",
               tpClassBound = Just (RefClassType (ClassType {
                   ctsClassName = "java/lang/Object",
                   ctsTypeArguments = []
               })),
               tpInterfaceBound = []
           }
       ]
  it "can handle the class signature of Iterator" $ do
   let sig = "<E:Ljava/lang/Object;>Ljava/lang/Object;"
   parseOnly classSignatureP sig `shouldBe`
       Right (ClassSignature {
           csTypeParameters = [
               TypeParameter {
                   tpIndentifier = "E",
                   tpClassBound = Just (RefClassType (ClassType {
                       ctsClassName = "java/lang/Object",
                       ctsTypeArguments = []
                   })),
                   tpInterfaceBound = []
               }
           ],
           csSuperclassSignature = ClassType {
               ctsClassName = "java/lang/Object",
               ctsTypeArguments = []
           },
           csInterfaceSignatures = []
       })
  it "can handle the method signature of Iterator" $ do
    let sig = "(Ljava/util/function/Consumer<-TE;>;)V"
    parseOnly methodSignatureP sig `shouldBe`
      Right (MethodSignature {
          msTypeParameters = [],
          msArguments = [
              ReferenceType (RefClassType (ClassType {
                  ctsClassName = "java/util/function/Consumer",
                  ctsTypeArguments = [
                      Just (TypeArgument {
                          taWildcard = Just WildMinus,
                          taType = RefTypeVariable (TypeVariable {
                              tvAsText = "E"
                          })
                      })
                  ]
              }))
          ],
          msResults = Nothing,
          msThrows = []
      })


  it "can handle the simple method signature" $ do
    parseOnly methodSignatureP "()V" `shouldBe` Right
        ( MethodSignature [] [] Nothing [])

  it "can handle throws method signature" $ do
    parseOnly (methodSignatureP <* endOfInput) "()V^TE;" `shouldBe` Right
        ( MethodSignature [] [] Nothing [ThrowsTypeVariable (TypeVariable "E")])

  it "can handle throws method signature with class" $ do
    parseOnly (methodSignatureP) "()V^Ljava/lang/Exception;" `shouldBe` Right
        ( MethodSignature [] [] Nothing [
                ThrowsClass (
                    ClassType {
                        ctsClassName = "java/lang/Exception"
                        , ctsTypeArguments = []
                        }
                    )
            ])

  it "can parse a type variable" $ do
    parseOnly typeVariableP "TE;" `shouldBe` Right (TypeVariable "E")

  it "can handle the simple field signature" $ do
    parseOnly fieldSignatureP "Ljava/util/function/Consumer<-TE;>;" `shouldBe` Right
        ( FieldSignature
          (RefClassType
           (ClassType "java/util/function/Consumer"
            [Just (TypeArgument (Just WildMinus) . RefTypeVariable $ TypeVariable "E") ]
           )))


instance Arbitrary (Signature High) where
  arbitrary = genericArbitraryU

instance Arbitrary (FieldSignature) where
  arbitrary = genericArbitraryU

instance Arbitrary (ReferenceType) where
  arbitrary = do
    n <- getSize
    if n == 0
      then pure (RefTypeVariable $ TypeVariable "X")
      else scale (\n -> n `div` 2) $ genericArbitraryU

instance Arbitrary (ClassType) where
  arbitrary = do
    s <- getSize
    n <- choose (0, s)
    x <- vectorOf n (resize (s `div` n) arbitrary)
    oneof
      [ ClassType <$> arbitrary <*> pure x
      , InnerClassType <$> elements ["a", "subclass"] <*> resize (n `div` 2) arbitrary <*> pure x
      ]


instance Arbitrary (TypeVariable) where
  arbitrary = TypeVariable <$> elements ["A","B", "C", "HJ"]

instance Arbitrary (TypeSignature) where
  arbitrary = do
    n <- getSize
    if n == 0
    then BaseType <$> arbitrary
    else resize (n `div` 2) $ ReferenceType <$> arbitrary

instance Arbitrary (TypeArgument) where
  arbitrary = genericArbitraryU

instance Arbitrary (TypeParameter) where
  arbitrary = TypeParameter <$> elements ["A","B", "C", "HJ"] <*> arbitrary <*> arbitrary

instance Arbitrary (Wildcard) where
  arbitrary = genericArbitraryU

instance Arbitrary (MethodSignature) where
  arbitrary = genericArbitraryU

instance Arbitrary (ClassSignature) where
  arbitrary = genericArbitraryU

instance Arbitrary (ThrowsSignature) where
  arbitrary = genericArbitraryU

-- "Lcom/apple/eawt/_AppEventHandler$_AppEventDispatcher<Lcom/apple/eawt/QuitHandler;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/ScreenSleepListener;Lcom/apple/eawt/AppEvent$ScreenSleepEvent;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/SystemSleepListener;Lcom/apple/eawt/AppEvent$SystemSleepEvent;>;"
-- "Lcom/apple/eawt/_AppEventHandler$_BooleanAppEventMultiplexor<Lcom/apple/eawt/UserSessionListener;Lcom/apple/eawt/AppEvent$UserSessionEvent;>;"
