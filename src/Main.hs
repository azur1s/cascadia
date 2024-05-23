module Main where

import Syntax
import Parse
import Infer

import System.Environment
import Data.List (intercalate)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            src <- readFile file
            case runParser src file of
                Left err   -> print err
                Right tops -> do
                    putStrLn $ "Parsed       : "
                        ++ intercalate "\n               " (map fmtTop tops)
                    case runInfer $ inferTops tops of
                        Left err -> putStrLn err
                        Right topsT -> do
                            putStrLn $ "Inferred     : "
                                ++ intercalate "\n               " (map fmtTopT topsT)
        _ -> putStrLn "Invalid arguments. Usage: cascadia <file>"