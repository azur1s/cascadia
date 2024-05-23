{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Infer where

import Syntax
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Bifunctor
import Data.List

tyVar :: Int -> Type
tyVar i = TVar $ ([1..] >>= flip replicateM ['a'..'z']) !! i

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
    apply _ (LitT l)          = LitT l
    apply s (VarT v t)        = VarT v (apply s t)
    apply s (AppT f args t)   = AppT (apply s f) (map (apply s) args) (apply s t)
    apply s (LamT params e t) = LamT (map (second (apply s)) params) (apply s e) (apply s t)
    apply s (OpsT op l r t)   = OpsT op (apply s l) (apply s r) (apply s t)

    ftv (LitT _)          = []
    ftv (VarT _ t)        = ftv t
    ftv (AppT f args t)   = ftv f ++ concatMap ftv args ++ ftv t
    ftv (LamT params e t) = ftv (map snd params) ++ ftv e ++ ftv t
    ftv (OpsT _ l r t)    = ftv l ++ ftv r ++ ftv t

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
type Infer a = StateT Int (Except String) a

runInfer :: Infer a -> Either String a
runInfer m = runExcept $ evalStateT m 0

newFresh :: Infer Type
newFresh = do
    i <- get
    put $ i + 1
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

infer :: TypeEnv -> Expr -> Infer (ExprT, Type, Subst)
infer _ (Lit l) = return (LitT l, litType l, nullSubst)
    where
        litType (LInt _)  = TInt
        litType (LBool _) = TBool

infer env (Var v) = do
    t <- lookupEnv env v
    return (VarT v t, t, nullSubst)

infer env (App f args) = do
    -- Infer the type of the function
    (f', ft, s1) <- infer env f
    -- Infer the types of the arguments
    (at, ts, s2) <- foldM
        (\(args', ts, s) arg -> do
            (arg', at, s') <- infer (apply s env) arg
            return (args' ++ [arg'], ts ++ [at], s' `composeSubst` s))
        ([], [], nullSubst) args

    t <- newFresh -- Return type of the application
    let farr = foldr TArrow t ts
    s3 <- case unify (apply s2 ft) farr of
        Just s  -> return s
        Nothing -> throwError $ "Type mismatch: Application expected "
                ++ fmtType (apply s2 ft) ++ " but got " ++ fmtType farr

    let t' = apply s3 t
    return (AppT (apply s3 f') (map (apply s3) at) t',
        t', s3 `composeSubst` s2 `composeSubst` s1)

infer env (Lam params e) = do
    -- Create a new type variable for each parameter
    ts <- mapM (const newFresh) params
    -- Create a new environment with the parameters and their types
    let env' = env ++ zip (map fst params) (map (Forall []) ts)
    -- Infer the type of the lambda body
    (e', t, s) <- infer env' e
    -- Create a function type from the parameter types to the body type
    let ts' = map (apply s) ts
        t' = foldr TArrow (apply s t) ts'
    return (LamT (zip (map fst params) ts') e' t', t', s)

infer env (Ops op l r) = do
    -- Infer the types of the left and right operands
    (l', lt, s1) <- infer env l
    (r', rt, s2) <- infer (apply s1 env) r
    -- Make sure that the types of the operands match the expected types
    expected <- case op of
        "+"  -> return TInt
        "-"  -> return TInt
        "*"  -> return TInt
        "/"  -> return TInt
        "==" -> return TInt
        "&&" -> return TBool
        "||" -> return TBool
        _    -> throwError $ "unknown operator " ++ op
    s3 <- case unify (apply s2 lt) expected of
        Just s  -> return s
        Nothing -> throwError $ "Type mismatch: expected " ++ fmtType expected
                ++ " but got " ++ fmtType (apply s2 lt)
    -- Make sure that both operands have the same type
    s4 <- case unify (apply s3 rt) (apply s3 lt) of
        Just s  -> return s
        Nothing -> throwError $ "Type mismatch: expected " ++ fmtType (apply s3 lt)
                ++ " but got " ++ fmtType (apply s3 rt)
    return (OpsT op (apply s4 l') (apply s4 r') (apply s4 lt),
        apply s4 lt, s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1)