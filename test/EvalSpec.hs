module EvalSpec (spec) where

import           Control.Monad
import           Error
import           Eval
import           Reader
import           Test.Hspec

subfn :: String -> String
subfn str = do
    evaled <- return $ liftM show $ readExpr str >>= eval
    extractValue $ trapError evaled

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
    describe "boolbinop" $ do
        it "boolbinop num 001" $
            subfn "(< 3 4)" `shouldBe` "#t"
        it "boolbinop num 002" $
            subfn "(< 4 3)" `shouldBe` "#f"
        it "boolbinop num 003" $
            subfn "(<= 3 3)" `shouldBe` "#t"
        it "boolbinop str 001" $
            subfn "(string=? \"test\" \"test\")" `shouldBe` "#t"
        it "boolbinop str 002" $
            subfn "(string=? \"test\" \"tesu\")" `shouldBe` "#f"
        it "boolbinop str 003" $
            subfn "(string>? \"testt\" \"test\")" `shouldBe` "#t"
        it "boolbinop str 004" $
            subfn "(string<? \"testt\" \"test\")" `shouldBe` "#f"
        it "boolbinop str 005" $
            subfn "(string<=? \"test\" \"test\")" `shouldBe` "#t"
        it "boolbinop bool 001" $ do
            subfn "(&& #t #t)" `shouldBe` "#t"
            subfn "(&& #f #t)" `shouldBe` "#f"
            subfn "(|| #t #t)" `shouldBe` "#t"
            subfn "(|| #f #t)" `shouldBe` "#t"
            subfn "(|| #f #f)" `shouldBe` "#f"
    describe "if" $ do
        it "if 001" $ do
            subfn "(if (> 2 3) \"yes\" \"no\")" `shouldBe` "\"no\""
            subfn "(if (< 2 3) \"yes\" \"no\")" `shouldBe` "\"yes\""
            subfn "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")" `shouldBe` "9"
    describe "list op" $ do
        it "car 001" $ do
            subfn "(car '(a b c))" `shouldBe` "a"
            subfn "(car '(a))" `shouldBe` "a"
            subfn "(car '(a b . c))" `shouldBe` "a"
            subfn "(car 'a)" `shouldNotBe` "()"
            subfn "(car 'a 'b)" `shouldNotBe` "()"
        it "cdr 001" $ do
            subfn "(cdr '(a b c))" `shouldBe` "(b c)"
            subfn "(cdr '(a))" `shouldBe` "()"
            subfn "(cdr '(a b . c))" `shouldBe` "(b . c)"
            subfn "(cdr 'a)" `shouldNotBe` "()"
            subfn "(cdr 'a 'b)" `shouldNotBe` "()"
        it "cons 001" $ do
            subfn "(cons 'a '())" `shouldBe` "(a)"
            --subfn "(cons 'a ())" `shouldBe` "(a)"
            subfn "(cons 'a 'b)" `shouldBe` "(a . b)"
            subfn "(cons 'a '(b))" `shouldBe` "(a b)"
            subfn "(cons '(a) 'b)" `shouldBe` "((a) . b)"
            subfn "(cons 'a '(b c))"  `shouldBe` "(a b c)"
            subfn "(cons 'a '(b . c))"  `shouldBe` "(a b . c)"
            subfn "(cons '(a) '(b . c))"  `shouldBe` "((a) b . c)"


