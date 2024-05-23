module Syntax where

import Data.List

data Lit
    = LInt Int
    | LBool Bool
    deriving (Show, Eq)

fmtLit :: Lit -> String
fmtLit (LInt i)  = show i
fmtLit (LBool b) = show b

data Expr
    = Lit Lit
    | Var String
    | App Expr [Expr]
    | Lam [(String, Maybe Type)] Expr
    | Ops String Expr Expr
    deriving (Show, Eq)

fmtExpr :: Expr -> String
fmtExpr (Lit l)          = fmtLit l
fmtExpr (Var v)          = v
fmtExpr (App f args)     = f' ++ "(" ++ intercalate ", " (map fmtExpr args) ++ ")"
    where f' = case f of
            Var v -> v
            _     -> "(" ++ fmtExpr f ++ ")"
fmtExpr (Lam params e)   = "\\" ++ unwords [v ++ maybe "" ((": " ++) . fmtType) t | (v, t) <- params] ++ ". " ++ fmtExpr e
fmtExpr (Ops op l r)     = fmtExpr l ++ " " ++ op ++ " " ++ fmtExpr r

-- | Typed expression
data ExprT
    = LitT Lit
    | VarT String Type
    | AppT ExprT [ExprT] Type
    | LamT [(String, Type)] ExprT Type
    | OpsT String ExprT ExprT Type
    deriving (Show, Eq)

fmtExprT :: ExprT -> String
fmtExprT (LitT l)          = fmtLit l
fmtExprT (VarT v _)        = v
fmtExprT (AppT f args _)   = f' ++ "(" ++ intercalate ", " (map fmtExprT args) ++ ")"
    where f' = case f of
            VarT v _ -> v
            _        -> "(" ++ fmtExprT f ++ ")"
fmtExprT (LamT params e _) = "\\" ++ unwords [v ++ " : " ++ fmtType t | (v, t) <- params] ++ ". " ++ fmtExprT e
fmtExprT (OpsT op l r _)   = fmtExprT l ++ " " ++ op ++ " " ++ fmtExprT r

-- | Types
data Type
    = TInt
    | TBool
    | TVar String
    | TArrow Type Type
    deriving (Show, Eq)

fmtType :: Type -> String
fmtType TInt         = "Int"
fmtType TBool        = "Bool"
fmtType (TVar v)     = v
fmtType (TArrow l r) = fmtType l ++ " -> " ++ fmtType r