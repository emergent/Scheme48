module EvalSpec (spec) where

import Test.Hspec
import Eval
import Reader
import Error
import Control.Monad

subfn :: String -> String
subfn str = do
    evaled <- return $ liftM show $ readExpr str >>= eval
    r <- extractValue $ trapError evaled
    return r

spec :: Spec
spec = do
    describe "eval" $ do
        it "calc 001" $
            subfn "(+ 1 2)" `shouldBe` "3"
        it "calc 002" $
            subfn "(+ 1 (- 3 2))" `shouldBe` "2"
        it "calc 003" $
            subfn "(- (+ 4 6 3) 3 5 2)" `shouldBe` "3"
    describe "error" $ do
        it "error 001" $
            subfn "(+ 2 \"two\")" `shouldBe` "Invalid type: expected number, found \"two\""
        it "error 002" $
            subfn "(+ 2)" `shouldBe` "Expected 2 args; found values 2"
        it "error 003" $
            subfn "(what? 2)" `shouldBe` "Unrecognized primitive function args: \"what?\""
