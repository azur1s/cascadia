module CPS where

import Syntax

import Control.Monad.Except
import Control.Monad.State
import Data.Functor

type CPS a = StateT Int (Except String) a

runCPS :: CPS a -> Either String a
runCPS m = runExcept $ evalStateT m 0

newFresh :: CPS Int
newFresh = do
    i <- get
    put (i + 1)
    return i

fmtFresh :: Int -> String
fmtFresh = ("k" ++) . show

cpsExpr :: ExprT -> AtomCPS -> CPS ExprCPS
cpsExpr (LitT n t) k = return $ AppCPS k [LitCPS n t]
cpsExpr (VarT x t) k = return $ AppCPS k [VarCPS x t]

cpsExpr e _ = error $ "TODO " ++ show e
