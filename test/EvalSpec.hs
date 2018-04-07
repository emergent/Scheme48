module EvalSpec (spec) where

import Test.Hspec
import Eval
import Reader

spec :: Spec
spec = do
    describe "eval" $ do
        it "calc 001" $
            (show . eval . readExpr) "(+ 1 2)" `shouldBe` "3"
        it "calc 002" $
            (show . eval . readExpr) "(+ 1 (- 3 2))" `shouldBe` "2"
