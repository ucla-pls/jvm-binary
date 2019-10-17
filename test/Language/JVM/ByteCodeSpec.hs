{-# Language OverloadedStrings #-}
module Language.JVM.ByteCodeSpec where

import SpecHelper
import Data.Either

-- containers
import qualified Data.IntMap as IntMap

import Language.JVM.ByteCode
import Language.JVM.ClassFileReader
import Language.JVM.ConstantPool
import Language.JVM.Constant

spec :: Spec
spec = do
  describe "new RefType" $ do
    it "should evolve New" $ do
        let cp = ConstantPool (IntMap.singleton 0 (CClassRef "[J"))
        let x = runEvolve (EvolveConfig [] cp (const True))
                (evolveBC undefined (ByteCodeInst 0 (New (Reference 0 1))))
        x `shouldBe` Right (ByteCodeInst 0 (New "[J"))

    it "should evolve New" $ do
        let cp = ConstantPool (IntMap.singleton 0 (CClassRef "java/lang/Object"))
        let x = runEvolve (EvolveConfig [] cp (const True))
                (evolveBC undefined (ByteCodeInst 0 (New (Reference 0 1))))
        x `shouldBe` Right (ByteCodeInst 0 (New "[Ljava/lang/Object;"))

    it "should roundtrip New" $ do
        let x = byteCodeRoundtrip (ByteCodeInst 0 (New "[J"))
        x `shouldSatisfy` isRight

    it "should roundtrip New" $ do
        let x = byteCodeRoundtrip (ByteCodeInst 0 (New "[Ljava/lang/Object;"))
        x `shouldSatisfy` isRight



-- runEvolve :: EvolveConfig -> Evolve a -> Either ClassFileError a
