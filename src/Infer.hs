{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Infer where

import Header
import Syntax
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Bifunctor
import Data.List

tyLetters :: Int -> String
tyLetters i = "'" ++ ([1..] >>= flip replicateM ['a'..'z']) !! i

tyVar :: Int -> Type
tyVar i = TVar $ tyLetters i

-- | Scheme
data Scheme where
  Forall :: [String] -> Type -> Scheme
  deriving (Show, Eq)

-- | Substitution
type Subst = [(String, Type)]

nullSubst :: Subst
nullSubst = []

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

-- | The `Types` class represents types that support substitution and free type
-- variable extraction
class Types a where
    -- | Apply a substitution to that type
    apply :: Subst -> a -> a
    -- | Get all free type variables in that type
    ftv :: a -> [String]

instance Types Type where
    apply s (TVar u) = case lookup u s of
        Just t  -> t
        Nothing -> TVar u
    apply s (TArrow t1 t2) = TArrow (apply s t1) (apply s t2)
    apply _ t = t

    ftv (TVar u) = [u]
    ftv (TArrow t1 t2) = ftv t1 ++ ftv t2
    ftv _ = []

instance Types [Type] where
    apply = map . apply
    ftv   = nub . concatMap ftv

instance Types Scheme where
    -- Apply a substitution to a scheme by only substituting the types that are
    -- not quantified
    -- Example: ∀a b. a -> b -> c, [a -> Int, c -> Int] = ∀a b. a -> b -> Int
    apply s (Forall as t) = Forall as $ apply s' t
        where s' = filter (\(u, _) -> u `notElem` as) s

    -- Get free type variables by removing all quantified variables `as` from
    -- the free type variables of the inner type `t`
    ftv (Forall as t) = ftv t \\ as

instance Types ExprT where
    apply _ (LitT l t)         = LitT l t
    apply s (VarT v t)         = VarT v (apply s t)
    apply s (AppT f e t)       = AppT (apply s f) (apply s e) (apply s t)
    apply s (LamT (x, xt) e t) = LamT (x, apply s xt) (apply s e) (apply s t)
    apply s (UnaT op e t)      = UnaT op (apply s e) (apply s t)
    apply s (BinT op l r t)    = BinT op (apply s l) (apply s r) (apply s t)
    apply s (IfT c t e t')     = IfT (apply s c) (apply s t) (apply s e) (apply s t')

    ftv (LitT _ _)         = []
    ftv (VarT _ t)         = ftv t
    ftv (AppT f e t)       = ftv f ++ ftv e ++ ftv t
    ftv (LamT (_, xt) e t) = ftv xt ++ ftv e ++ ftv t
    ftv (UnaT _ _ t)       = ftv t
    ftv (BinT _ l r t)     = ftv l ++ ftv r ++ ftv t
    ftv (IfT c t e t')     = ftv c ++ ftv t ++ ftv e ++ ftv t'

type TypeEnv = [(String, Scheme)]

instance Types TypeEnv where
    apply s env = [(n, apply s' t) | (n, t) <- env]
        -- Filter out any substitution that is not in the environment to avoid
        -- clashes with quantified variables
        -- Example:
        --   env   = x = ∀a. a -> b, y = ∀. c
        --   subst = [a -> Int, b -> Bool, c -> Int]
        --   If we apply the substitution directly, we would get:
        --     x = Int -> Bool, y = Int
        --   Which is incorrect because `a` is quantified in `x`
        --   So we filter out `a -> Int` and only apply `b -> Bool` to `x`
        where s' = filter (\(n, _) -> n `notElem` map fst env) s

    ftv env = nub $ concatMap (ftv . snd) env

remove :: TypeEnv -> String -> TypeEnv
remove env v = filter (\(u, _) -> u /= v) env

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
    where as = ftv t \\ ftv env

-- | Unifies two types and return a substitution that makes them equal, if
-- unification is not possible, return `Nothing`
unify :: Type -> Type -> Maybe Subst
unify TInt  TInt  = Just nullSubst
unify TBool TBool = Just nullSubst
unify (TVar u) t = bind u t
unify t (TVar u) = bind u t
unify (TArrow l r) (TArrow l' r') = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return $ s2 `composeSubst` s1
unify _ _ = Nothing

-- | Bind a type variable to a type, if the type variable is already bound to a
-- type, make sure they are equal
bind :: String -> Type -> Maybe Subst
bind u t | t == TVar u    = Just nullSubst
         | u `elem` ftv t = Nothing
         | otherwise      = Just [(u, t)]

-- | Type inference monad
type Infer a = StateT (Int, TypeEnv) (Except String) a

getCounter :: Infer Int
getCounter = gets fst

putCounter :: Int -> Infer ()
putCounter i = modify $ first (const i)

getEnv :: Infer TypeEnv
getEnv = gets snd

putEnv :: TypeEnv -> Infer ()
putEnv env = modify $ second (const env)

applyEnv :: Subst -> Infer ()
applyEnv s = getEnv >>= putEnv . apply s

runInferM :: Infer a -> Either String a
runInferM m = runExcept $ evalStateT m (0, [])

runInfer :: [Top] -> Result [TopT]
runInfer tops = case runInferM $ inferTops tops of
    Left err    -> Err $ InferError err
    Right topsT -> Yay topsT

runInferExpr :: Expr -> Result ExprT
runInferExpr e = case runInferM $ infer e of
    Left  err        -> Err $ InferError err
    Right (e', _, _) -> Yay e'

newFresh :: Infer Type
newFresh = do
    i <- getCounter
    putCounter (i + 1)
    return $ tyVar i

-- | Instantiate a polymorphic type scheme with fresh type variables
-- Example: ∀a. a -> b, instantiate = [a -> α1, b -> α2] => α1 -> α2
instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const newFresh) as
    let s = zip as as'
    return $ apply s t

lookupEnv :: TypeEnv -> String -> Infer Type
lookupEnv env v = case lookup v env of
    Just s  -> instantiate s
    Nothing -> throwError $ "unbound variable " ++ v

infer :: Expr -> Infer (ExprT, Type, Subst)
infer (Lit l) = return (LitT l t, t, nullSubst)
    where t = case l of
            LInt  _ -> TInt
            LBool _ -> TBool

infer (Var v) = do
    env <- getEnv
    t <- lookupEnv env v
    return (VarT v t, t, nullSubst)

infer (App f x) = do
    -- Infer the type of the function
    (f', ft, s1) <- infer f
    -- Infer the types of the argument
    (x', xt, s2) <- infer x

    t <- newFresh -- Return type of the application
    let farr = TArrow t xt
    s3 <- case unify (apply s2 ft) farr of
        Just s  -> return s
        Nothing -> throwError $ "Type mismatch: Application expected "
                ++ fmtType (apply s2 ft) ++ " but got " ++ fmtType farr

    let t' = apply s3 t
    return (AppT (apply s3 f') (apply s3 x') t',
        t', s3 `composeSubst` s2 `composeSubst` s1)

infer (Lam x t e) = do
    -- Create a new type variable for parameter
    t <- maybe newFresh return t
    -- Create a new environment with the parameter type
    env <- getEnv
    let env' = env ++ [(x, Forall [] t)]
    putEnv env'
    -- Infer the type of the lambda body
    (e', t, s) <- infer e
    putEnv env
    -- Create a function type from the parameter types to the body type
    let t'  = apply s t
        tf' = TArrow (apply s t) t'
    return (LamT (x, t') e' tf', tf', s)

infer (Una op e) = do
    (e', t, s) <- infer e
    applyEnv s

    (et, ret) <- case op of
        "-" -> return (TInt, TInt)
        "!" -> return (TBool, TBool)
        _   -> throwError $ "unknown operator " ++ op

    s' <- case unify et t of
        Just s  -> return s
        Nothing -> throwError $ "Type mismatch: Operator " ++ op
                ++ " expected " ++ fmtType et ++ " but got " ++ fmtType t

    let t' = apply s' ret
    return (UnaT op (apply s' e') t', t', s' `composeSubst` s)

infer (Bin op l r) = do
    -- Infer the types of the left and right operands
    (l', lt, s1) <- infer l
    applyEnv s1
    (r', rt, s2) <- infer r
    applyEnv s2

    (el, er, ret) <- case op of
        op | op `elem` ["+", "-", "*", "/"]   -> return (TInt, TInt, TInt)
           | op `elem` ["==", "!="]           -> newFresh >>= \t -> return (t, t, TBool)
           -- TODO: Equality Typeclass
           | op `elem` ["<", "<=", ">", ">="] -> return (TInt, TInt, TBool)
           | op `elem` ["&&", "||"]           -> return (TBool, TBool, TBool)
        _ -> throwError $ "unknown operator " ++ op

    s3 <- either throwError return (checkOperand lt el)
    applyEnv s3
    s4 <- either throwError return (checkOperand rt er)
    applyEnv s4

    let t = apply s4 ret
    return (BinT op (apply s4 l') (apply s4 r') t, t,
        s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1)
    where
        checkOperand x t = case unify x t of
            Just s  -> Right s
            Nothing -> Left $ "Type mismatch: Operator " ++ op
                    ++ " expected " ++ fmtType t ++ " but got "
                    ++ fmtType x

infer (If c t e) = do
    -- Infer the type of the condition
    (c', ct, s1) <- infer c
    applyEnv s1
    -- Infer the types of the then and else branches
    (t', tt, s2) <- infer t
    applyEnv s2
    (e', et, s3) <- infer e
    applyEnv s3

    s4 <- either throwError return (checkOperand ct TBool)
    applyEnv s4
    s5 <- either throwError return (checkOperand tt et)
    applyEnv s5

    let t = apply s5 tt
    return (IfT (apply s5 c') (apply s5 t') (apply s5 e') t, t,
        s5 `composeSubst` s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1)
    where
        checkOperand x t = case unify x t of
            Just s  -> Right s
            Nothing -> Left $ "Type mismatch: If condition expected"
                    ++ fmtType TBool ++ " but got " ++ fmtType x

inferTop :: Top -> Infer (TopT, Type, Subst)
inferTop (Decl n e) = do
    t <- newFresh

    env <- getEnv
    let env' = env ++ [(n, Forall [] t)]
    putEnv env'

    (e', t', s) <- infer e
    applyEnv s

    s' <- case unify t t' of
        Just s  -> return s
        Nothing -> throwError $ "Type mismatch: Declaration " ++ n
                ++ " expected " ++ fmtType t ++ " but got " ++ fmtType t'
    applyEnv s'

    let t'' = apply s' t'
    let s' = generalize (apply s env) t''
    putEnv $ (n, s') : env

    return (DeclT n e' t'', t'', s)

inferTops :: [Top] -> Infer [TopT]
inferTops tops = do
    (tops', _) <- foldM
        (\(tops', s) top -> do
            applyEnv s
            (top', _, s') <- inferTop top
            return (tops' ++ [top'], s' `composeSubst` s))
        ([], nullSubst) tops
    return tops'