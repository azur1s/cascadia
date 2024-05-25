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
    | App Expr Expr
    | Lam String (Maybe Type) Expr
    | Una String Expr
    | Bin String Expr Expr
    | If Expr Expr Expr
    deriving (Show, Eq)

fmtExpr :: Expr -> String
fmtExpr (Lit l)      = fmtLit l
fmtExpr (Var v)      = v
fmtExpr (App f e)    = f' ++ " " ++ fmtExpr e
    where f' = case f of
            Var _ -> fmtExpr f ++ " "
            _     -> "(" ++ fmtExpr f ++ ") "
fmtExpr (Lam x t e)  = "\\" ++ x' ++ " -> " ++ fmtExpr e
    where x' = case t of
            Just t' -> x ++ " : " ++ fmtType t'
            Nothing -> x
fmtExpr (Una op e)   = op ++ fmtExpr e
fmtExpr (Bin op l r) = fmtExpr l ++ " " ++ op ++ " " ++ fmtExpr r
fmtExpr (If c t e)   = "if " ++ fmtExpr c ++ " then " ++ fmtExpr t ++ " else " ++ fmtExpr e

-- | Typed expression
data ExprT
    = LitT Lit Type
    | VarT String Type
    | AppT ExprT ExprT Type
    | LamT (String, Type) ExprT Type
    | UnaT String ExprT Type
    | BinT String ExprT ExprT Type
    | IfT ExprT ExprT ExprT Type
    deriving (Show, Eq)

fmtExprT :: ExprT -> String
fmtExprT (LitT l _)        = fmtLit l
fmtExprT (VarT v _)        = v
fmtExprT (AppT f e _)      = f' ++ " " ++ fmtExprT e
    where f' = case f of
            VarT v _ -> v
            _        -> "(" ++ fmtExprT f ++ ")"
fmtExprT (LamT (x, t) e _) = "\\" ++ x ++ " : " ++ fmtType t ++ " -> " ++ fmtExprT e
fmtExprT (UnaT op e _)     = op ++ fmtExprT e
fmtExprT (BinT op l r _)   = fmtExprT l ++ " " ++ op ++ " " ++ fmtExprT r
fmtExprT (IfT c t e _)     = "if " ++ fmtExprT c ++ " then " ++ fmtExprT t ++ " else " ++ fmtExprT e

typeOfExprT :: ExprT -> Type
typeOfExprT (LitT _ t)     = t
typeOfExprT (VarT _ t)     = t
typeOfExprT (AppT _ _ t)   = t
typeOfExprT (LamT _ _ t)   = t
typeOfExprT (UnaT _ _ t)   = t
typeOfExprT (BinT _ _ _ t) = t
typeOfExprT (IfT  _ _ _ t) = t

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
fmtType (TArrow l r) = fmt' l ++ " -> " ++ fmtType r
    where fmt' x = case x of
            TArrow _ _ -> "(" ++ fmtType x ++ ")"
            _          -> fmtType x

-- | Expression with CPS
data AtomCPS
    = LitCPS Lit Type
    | VarCPS String Type
    -- | AppCPS ExprCPS [ExprCPS] Type
    | LamCPS (String, Type) ExprCPS Type
    | UnaCPS String AtomCPS Type
    | BinCPS String AtomCPS AtomCPS Type
    | IfCPS AtomCPS AtomCPS AtomCPS Type
    deriving (Show, Eq)

data ExprCPS
    = HaltCPS AtomCPS
    | AppCPS AtomCPS [AtomCPS]
    deriving (Show, Eq)

fmtAtomCPS :: AtomCPS -> String
fmtAtomCPS (LitCPS l _)        = fmtLit l
fmtAtomCPS (VarCPS v _)        = v
fmtAtomCPS (LamCPS (x, t) e _) = "\\" ++ x ++ " : " ++ fmtType t ++ " -> " ++ fmtExprCPS e
fmtAtomCPS (UnaCPS op e _)     = op ++ fmtAtomCPS e
fmtAtomCPS (BinCPS op l r _)   = fmtAtomCPS l ++ " " ++ op ++ " " ++ fmtAtomCPS r
fmtAtomCPS (IfCPS c t e _)     = "if " ++ fmtAtomCPS c ++ " then " ++ fmtAtomCPS t ++ " else " ++ fmtAtomCPS e

fmtExprCPS :: ExprCPS -> String
fmtExprCPS (HaltCPS a)    = fmtAtomCPS a
fmtExprCPS (AppCPS f args) = f' ++ "(" ++ intercalate ", " (map fmtAtomCPS args) ++ ")"
    where f' = case f of
            VarCPS v _ -> v
            _          -> "(" ++ fmtAtomCPS f ++ ")"

-- | Top-level statements
data Top
    = Decl String Expr
    deriving (Show, Eq)

fmtTop :: Top -> String
fmtTop (Decl v e) = v ++ " = " ++ fmtExpr e

data TopT
    = DeclT String ExprT Type
    deriving (Show, Eq)

fmtTopT :: TopT -> String
fmtTopT (DeclT v e t) = v ++ " : " ++ fmtType t ++ " = " ++ fmtExprT e
