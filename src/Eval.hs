module Eval (eval) where

import Control.Monad.Error
import Value
import Error

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form"  badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
            ($ args)
            (lookup func primitives)

-- TODO 4-1: add type-testing primitives
-- TODO 4-3: add symbol-handling functions
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
            ("-", numericBinop (-)),
            ("*", numericBinop (*)),
            ("/", numericBinop div),
            ("mod", numericBinop mod),
            ("quotient", numericBinop quot), -- 商
            ("remainder", numericBinop rem) -- 余り
            --("symbol?", boolBinop )
            ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

-- TODO 4-2: change unpackNum to returning 0 when list or strings
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                if null parsed
                    then throwError $ TypeMismatch "number" $ String n
                    else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" $ notNum