module Main where

import Syntax
import Parse
import Infer

main :: IO ()
main = do

  let test = "(\\f -> f 1)(\\x -> x)"

  case runParser test of
      Left err   -> print err
      Right pexp -> do
        putStrLn $ "Parsed: " ++ fmtExpr pexp
        putStrLn $ "Inferring: " ++ fmtExpr pexp
        case runInfer $ infer [] pexp of
            Left err -> putStrLn err
            Right (e, t, s)
                -> putStrLn ("Inferred     : " ++ fmtExprT e)
                >> putStrLn ("Expr Type    : " ++ fmtType t)
                >> putStrLn ("Substitution : " ++ show s)