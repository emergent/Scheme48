module Main where

import System.Environment
import Control.Monad
import Reader
import Eval

main :: IO ()
main = do -- getArgs >>= print . eval . readExpr . head
    (a:_) <- getArgs
    putStrLn $ "input string: " ++ a
    (print . eval . readExpr) a
    --putStrLn $ readExpr a

