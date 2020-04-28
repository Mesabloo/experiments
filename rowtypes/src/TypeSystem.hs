{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module TypeSystem where

import Core
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)

type Environment = Map String Scheme

type Typecheck a = RWST Environment () Integer (Except String) a

runTypecheck :: Typecheck a -> Environment -> Either String a
runTypecheck check env = fst <$> evalRWST check env 0 & runExcept

class Freevars s where
    free :: s -> Set String

instance Freevars Type where
    free (TArrow t1 t2) = free t1 <> free t2
    free (TVar v)       = Set.singleton v
    free (TRecord row)  = free row
    free (TRow fs ext)  = foldMap free fs <> maybe mempty free ext
    free _              = mempty

instance (Freevars s, Functor f, Foldable f) => Freevars (f s) where
    free = foldMap free

type Subst = Map String Type

(•) :: Subst -> Subst -> Subst
s1 • s2 = fmap (s1 •>) s2 `Map.union` s1

class Substitute s where
    applySubst :: Subst -> s -> s

    (•>) :: Subst -> s -> s
    (•>) = applySubst

    (<•) :: s -> Subst -> s
    (<•) = flip (•>)

instance (Substitute s, Functor f) => Substitute (f s) where
    applySubst = fmap . applySubst

instance Substitute Type where
    applySubst sub (TArrow t1 t2) = TArrow (sub •> t1) (sub •> t2)
    applySubst sub (TRecord row)  = TRecord (sub •> row)
    applySubst sub (TRow fs ext)  = TRow (applySubst sub <$> fs) (applySubst sub <$> ext)
    applySubst sub var@(TVar v)   = fromMaybe var (Map.lookup v sub)
    applySubst _   t              = t

----------------------------------------------------------------------------------

fresh :: String -> Typecheck Type
fresh prefix = do
    count <- get
    let name = prefix <> show count
    modify \s -> s + 1
    pure (TVar name)

relax :: Type -> Type
relax (TRigid v)     = TVar v
relax (TArrow t1 t2) = TArrow (relax t1) (relax t2)
relax (TRecord row)  = TRecord (relax row)
relax (TRow fs ext)  = TRow (relax <$> fs) (relax <$> ext)
relax t              = t

instantiate :: Scheme -> Typecheck Type
instantiate (Forall vars ty) = do
    newVars <- mapM (const $ fresh "$") (Set.toList vars)
    let sub = Map.fromList (zip (Set.toList vars) newVars)
    pure (sub •> relax ty)

elabExpr :: Expr -> Typecheck (Expr, Type)
elabExpr e@EInt{}       = pure (e, TInt)
elabExpr e@EFloat{}     = pure (e, TFloat)
elabExpr e@EChar{}      = pure (e, TChar)
elabExpr e@(EId name)   =
    asks (Map.lookup name) >>= fmap (e,) . maybe (throwError (name <> " not found")) instantiate
elabExpr (EApp e1 e2)   = do
    (elab1, t1) <- elabExpr e1
    (elab2, t2) <- elabExpr e2
    t3          <- fresh "$"
    sub         <- unify t1 (TArrow t2 t3)
    pure (EApp elab1 elab2, sub •> t3)
elabExpr (ELam var e1) = do
    t2          <- fresh "$"
    (elab1, t1) <- local (Map.insert var (Forall mempty t2)) (elabExpr e1)
    pure (ELam var elab1, TArrow t2 t1)
elabExpr (ERecord fields) = do
    elab         <- traverse elabExpr fields
    let fieldsE = fst <$> elab
        fieldsT = snd <$> elab
        elabEx  = ERecord fieldsE
        ty      = TRecord (TRow fieldsT Nothing)
    pure (elabEx, ty)

--------------------------------------------------------------------------------

unify :: Type -> Type -> Typecheck Subst
unify t1 t2 | t1 == t2              = pure mempty
unify (TArrow t1 t2) (TArrow t3 t4) = unifyMany [t1, t2] [t3, t4]
unify (TVar v)       t              = tryBind v t
unify t              (TVar v)       = tryBind v t
unify (TRecord row1) (TRecord row2) = unify row1 row2
unify (TRow fs1 e1)  (TRow fs2 e2)  = do
  throwError "Not yet implemented! Come back later."
unify t1             t2             = throwError ("Could not unify " <> show t1 <> " and " <> show t2)

unifyMany :: [Type] -> [Type] -> Typecheck Subst
unifyMany []       []       = pure mempty
unifyMany (t1:ts1) (t2:ts2) = do
    sub  <- unify t1 t2
    sub' <- unifyMany (sub •> ts1) (sub •> ts2)
    pure (sub • sub')
unifyMany ts1      ts2      = error ("unifyMany: " <> show ts1 <> " vs " <> show ts2)

tryBind :: String -> Type -> Typecheck Subst
tryBind var ty
    | ty == TVar var     = pure mempty
    | occursCheck var ty = throwError ("Occurs check: " <> var <> " ~ " <> show ty)
    | otherwise          = pure (Map.singleton var ty)
  where occursCheck = (. free) . Set.member
