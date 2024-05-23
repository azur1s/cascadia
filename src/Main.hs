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
          case runInfer $ infer [] pexp of
            Left err -> putStrLn err
            Right (e, t, s)
              -> putStrLn ("Inferred     : " ++ fmtExprT e)
              >> putStrLn ("Expr Type    : " ++ fmtType t)
              >> putStrLn ("Substitution : " ++ show s)
    _ -> putStrLn "Invalid arguments. Usage: cascadia <file>"