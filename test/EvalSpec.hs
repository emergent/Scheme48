module EvalSpec (spec) where

import           Control.Monad
import           Eval
import           Reader
import           Test.Hspec
import           Types

subfn :: String -> IO String
subfn str = primitiveBindings >>= flip evalString str

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

spec :: Spec
spec = do
    describe "eval" $ do
        it "calc 001" $
            subfn "(+ 1 2)" `shouldReturn` "3"
        it "calc 002" $
            subfn "(+ 1 (- 3 2))" `shouldReturn` "2"
        it "calc 003" $
            subfn "(- (+ 4 6 3) 3 5 2)" `shouldReturn` "3"
    describe "error" $ do
        it "error 001" $
            subfn "(+ 2 \"two\")" `shouldReturn` "Invalid type: expected number, found \"two\""
        it "error 002" $
            subfn "(+ 2)" `shouldReturn` "Expected 2 args; found values 2"
        it "error 003" $
            subfn "(what? 2)" `shouldReturn` "Getting an unbound variable: : what?"
    describe "boolbinop" $ do
        it "boolbinop num 001" $
            subfn "(< 3 4)" `shouldReturn` "#t"
        it "boolbinop num 002" $
            subfn "(< 4 3)" `shouldReturn` "#f"
        it "boolbinop num 003" $
            subfn "(<= 3 3)" `shouldReturn` "#t"
        it "boolbinop str 001" $
            subfn "(string=? \"test\" \"test\")" `shouldReturn` "#t"
        it "boolbinop str 002" $
            subfn "(string=? \"test\" \"tesu\")" `shouldReturn` "#f"
        it "boolbinop str 003" $
            subfn "(string>? \"testt\" \"test\")" `shouldReturn` "#t"
        it "boolbinop str 004" $
            subfn "(string<? \"testt\" \"test\")" `shouldReturn` "#f"
        it "boolbinop str 005" $
            subfn "(string<=? \"test\" \"test\")" `shouldReturn` "#t"
        it "boolbinop bool 001" $ do
            subfn "(&& #t #t)" `shouldReturn` "#t"
            subfn "(&& #f #t)" `shouldReturn` "#f"
            subfn "(|| #t #t)" `shouldReturn` "#t"
            subfn "(|| #f #t)" `shouldReturn` "#t"
            subfn "(|| #f #f)" `shouldReturn` "#f"
    describe "if" $
        it "if 001" $ do
            subfn "(if (> 2 3) \"yes\" \"no\")" `shouldReturn` "\"no\""
            subfn "(if (< 2 3) \"yes\" \"no\")" `shouldReturn` "\"yes\""
            subfn "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")" `shouldReturn` "9"
    describe "list op" $ do
        it "car 001" $ do
            subfn "(car '(a b c))" `shouldReturn` "a"
            subfn "(car '(a))" `shouldReturn` "a"
            subfn "(car '(a b . c))" `shouldReturn` "a"
            subfn "(car 'a)" `shouldNotReturn` "()"
            subfn "(car 'a 'b)" `shouldNotReturn` "()"
        it "cdr 001" $ do
            subfn "(cdr '(a b c))" `shouldReturn` "(b c)"
            subfn "(cdr '(a))" `shouldReturn` "()"
            subfn "(cdr '(a b . c))" `shouldReturn` "(b . c)"
            subfn "(cdr 'a)" `shouldNotReturn` "()"
            subfn "(cdr 'a 'b)" `shouldNotReturn` "()"
        it "cons 001" $ do
            subfn "(cons 'a '())" `shouldReturn` "(a)"
            --subfn "(cons 'a ())" `shouldReturn` "(a)"
            subfn "(cons 'a 'b)" `shouldReturn` "(a . b)"
            subfn "(cons 'a '(b))" `shouldReturn` "(a b)"
            subfn "(cons '(a) 'b)" `shouldReturn` "((a) . b)"
            subfn "(cons 'a '(b c))"  `shouldReturn` "(a b c)"
            subfn "(cons 'a '(b . c))"  `shouldReturn` "(a b . c)"
            subfn "(cons '(a) '(b . c))"  `shouldReturn` "((a) b . c)"
        it "define vars" $ do -- test for define variables
            subfn "(define x 3)" `shouldReturn` "3"
            --subfn "(+ x 3)" `shouldReturn` "6"

