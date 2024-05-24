module Renamer where

import Syntax
import Infer (tyLetters)

import Control.Monad.State
import qualified Data.Map as M

type TyVar = String
type Renamer = State (M.Map TyVar TyVar, Int)

getOrCreateName :: TyVar -> Renamer TyVar
getOrCreateName v = do
    (m, i) <- get
    case M.lookup v m of
        Just v' -> return v'
        Nothing -> do
            let newv = tyLetters i
            put (M.insert v newv m, i + 1)
            return newv

renameType :: Type -> Renamer Type
renameType (TVar v) = TVar <$> getOrCreateName v
renameType (TArrow t1 t2) = TArrow <$> renameType t1 <*> renameType t2
renameType t = return t

renameExprT :: ExprT -> Renamer ExprT
renameExprT (VarT v t)        = VarT v <$> renameType t
renameExprT (AppT f args t)   = AppT <$> renameExprT f <*> mapM renameExprT args <*> renameType t
renameExprT (LamT params e t) = LamT <$> mapM (\(v, t') -> (,) v <$> renameType t') params <*> renameExprT e <*> renameType t
renameExprT (BinT op l r t)   = BinT op <$> renameExprT l <*> renameExprT r <*> renameType t
renameExprT e = return e

renameTop :: TopT -> Renamer TopT
renameTop (DeclT v e t) = DeclT v <$> renameExprT e <*> renameType t

renameTops :: [TopT] -> Renamer [TopT]
renameTops = mapM (\t -> put (M.empty, 0) >> renameTop t)

runRenamer :: [TopT] -> [TopT]
runRenamer tops = evalState (renameTops tops) (M.empty, 0)
