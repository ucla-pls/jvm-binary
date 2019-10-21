{-# Language OverloadedStrings #-}
module Language.JVM.ByteCodeSpec where

import SpecHelper
import Data.Either

-- containers
import qualified Data.IntMap as IntMap

import Language.JVM.ClassFileReader
import Language.JVM.ConstantPool
import Language.JVM.Constant
import Language.JVM.ByteCode

spec :: Spec
spec = do
  describe "new RefType" $ do
    it "should evolve 1, [J -> [[J" $ do
        let cp = ConstantPool (IntMap.singleton 0 (CClassRef "[J"))
        let x = runEvolve (EvolveConfig [] cp (const True))
                (evolveBC undefined
                 (ByteCodeInst 0 (NewArray (ArrayReference 0 1)))
                )
        x `shouldBe` Right (ByteCodeInst 0 (NewArray (NewArrayType 1 "[J")))

    it "should evolve 2, [[J -> [[J" $ do
        let cp = ConstantPool (IntMap.singleton 0 (CClassRef "[[J"))
        let x = runEvolve (EvolveConfig [] cp (const True))
                (evolveBC undefined
                 (ByteCodeInst 0 (NewArray (ArrayReference 0 2)))
                )
        x `shouldBe` Right (ByteCodeInst 0 (NewArray (NewArrayType 2 "J")))

    -- it "should evolve New" $ do
    --     let cp = ConstantPool (IntMap.singleton 0 (CClassRef "J"))
    --     let x = runEvolve (EvolveConfig [] cp (const True))
    --             (evolveBC undefined (ByteCodeInst 0 (NewArray (ArrayReference 0 1))))
    --     x `shouldBe` Right (ByteCodeInst 0 (NewArray (NewArrayType 1 "[J"))

    -- it "should evolve New" $ do
    --     let cp = ConstantPool (IntMap.singleton 0 (CClassRef "java/lang/Object"))
    --     let x = runEvolve (EvolveConfig [] cp (const True))
    --             (evolveBC undefined (ByteCodeInst 0 (New (Reference 0 1))))
    --     x `shouldBe` Right (ByteCodeInst 0 (New "[Ljava/lang/Object;"))

    it "should roundtrip NewArray" $ do
        let x = byteCodeRoundtrip (ByteCodeInst 0 (NewArray (NewArrayType 1 "J")))
        x `shouldSatisfy` isRight

    it "should roundtrip NewArray" $ do
        let x = byteCodeRoundtrip (ByteCodeInst 0 (NewArray (NewArrayType 2 "J")))
        x `shouldSatisfy` isRight



-- runEvolve :: EvolveConfig -> Evolve a -> Either ClassFileError a
