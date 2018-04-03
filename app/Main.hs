module Main where

import System.Environment
import Control.Monad
import Reader

expr :: Int -> Int -> Int
expr a b = a * b

main :: IO ()
main = do
    (a:_) <- getArgs
    putStrLn $ "input string: " ++ a
    putStrLn $ readExpr a

