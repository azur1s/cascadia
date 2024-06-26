module Main where

import Header
import Syntax
import Parse
import Infer
import Renamer
import CPS

import System.Environment
import Data.List (intercalate)

go :: String -> String -> Result [TopT]
go src path =
    case runParser src path >>= runInfer of
        Err err  -> Err err
        Yay tops -> Yay $ runRenamer tops

main :: IO ()
main = do
    case runParserExpr "(\\f x -> f x) (\\x -> x) 69" "<none>" >>= runInferExpr of
        Yay e   -> putStrLn $ fmtExprT e
        Err err -> putStrLn $ fmtErrKind err

    -- args <- getArgs
    -- case args of
    --     [file] -> do
    --         src <- readFile file
    --         case go src file of
    --             Yay tops -> putStrLn $ intercalate "\n" (map fmtTopT tops)
    --             Err err  -> putStrLn $ fmtErrKind err
    --     _ -> putStrLn "Invalid arguments. Usage: cascadia <file>"