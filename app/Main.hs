module Main where

import System.Environment
import Control.Monad
import Reader
import Eval
import Error

main :: IO ()
main = do -- getArgs >>= print . eval . readExpr . head
    (a:_) <- getArgs
    let evaled = liftM show $ readExpr a >>= eval
    putStrLn $ extractValue $ trapError evaled

