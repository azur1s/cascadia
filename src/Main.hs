module Main where

import Syntax
import Parse
import Infer

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            src <- readFile file
            case runParser src of
                Left err   -> print err
                Right pexp -> do
                    putStrLn $ "Parsed       : " ++ fmtExpr pexp
                        ++ "\n\ESC[2;37m               " ++ show pexp
                        ++ "\ESC[0m"
                    case runInfer $ infer [] pexp of
                        Left err -> putStrLn err
                        Right (e, t, s)
                            -> putStrLn ("Inferred     : " ++ fmtExprT e)
                            >> putStrLn ("Expr Type    : " ++ fmtType t)
                            >> putStrLn ("Substitution : " ++ show s)
        _ -> putStrLn "Invalid arguments. Usage: cascadia <file>"