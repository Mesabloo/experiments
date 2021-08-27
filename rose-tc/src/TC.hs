{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}

module TC where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except (MonadError, runExcept, throwError)
import Data.List (intersperse, intercalate, partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor (second)
import Control.Monad.RWS (MonadRWS, runRWST, evalRWST, put, get, asks, local, tell)
import Debug.Trace (trace)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Data.Functor ((<&>))

data Type
  = IntT
  | VarT Text
  | RigidT Text
  | ProductT Type
  | SumT Type
  | UnionT Type Type
  | ForallT [Text] Type
  | ConstraintT [Constr] Type
  | RowT [(Text, Type)]
  | FunT Type Type
  deriving (Eq)

data Constr
  = -- | @t1 ~ t2@
    EqC Type Type
  | -- | @z1 ⧀ z2@
    SubsetC Type Type
  deriving (Eq)

data Expr
  = LamE Text Expr
  | VarE Text
  | IntE Integer
  | SelectE Expr Text
  | CaseE Expr [(Pattern, Expr)]
  | RecordE [(Text, Expr)] (Maybe Text)
  | VariantE Text [Expr]
  | UnionE Expr Expr
  | PlusE Expr Expr
  | ProjE Expr

data Pattern
  = IntP Integer
  | VarP Text
  | WildcardP
  | ConstructorP Text [Pattern]
  | RecordP [(Text, Pattern)] Bool

instance Pretty Type where
  pretty IntT               = magenta $ text "int"
  pretty (VarT v)           = black . text $ Text.unpack v
  pretty (RigidT r)         = dullblue . text $ Text.unpack r
  pretty (UnionT t1 t2)     = parens $ pretty t1 <+> dullyellow (text "⊙") <+> pretty t2
  pretty (ProductT p)       = dullyellow (text "∏") <> pretty p
  pretty (SumT p)           = dullyellow (text "∑") <> pretty p
  pretty (ConstraintT cs t) = tupled (pretty <$> cs) <+> dullyellow (text "⇒") <+> pretty t
  pretty (ForallT v t)      = dullyellow (text "∀") <+> hsep (dullblue . text . Text.unpack <$> v) <> dot <+> pretty t
  pretty (RowT fields)      = encloseSep (white lbrace) (white rbrace) (white comma) $ showField <$> fields
    where showField (k, v) = dullcyan (text $ Text.unpack k) <+> colon <> colon <+> pretty v
  pretty (FunT t1 t2)       = parens (pretty t1) <+> dullyellow (text "→") <+> pretty t2

instance Pretty Constr where
  pretty (EqC t1 t2)     = pretty t1 <+> dullyellow (text "~") <+> pretty t2
  pretty (SubsetC t1 t2) = pretty t1 <+> dullyellow (text "⧀") <+> pretty t2

instance Pretty Expr where
  pretty (LamE v e)       = magenta (text "λ") <> white (text $ Text.unpack v) <> dot <+> pretty e
  pretty (VarE v)         = dullblue (text $ Text.unpack v)
  pretty (IntE i)         = dullred (integer i)
  pretty (SelectE e i)    = parens (pretty e) <> dullyellow dot <> dullcyan (text $ Text.unpack i)
  pretty (PlusE e1 e2)    = pretty e1 <+> dullyellow (text "+") <+> pretty e2
  pretty (UnionE e1 e2)   = pretty e1 <+> dullyellow (text "★") <+> pretty e2
  pretty (RecordE fs ext) = white lbrace <+> mconcat (punctuate (white comma) (showField <$> fs)) <> maybe empty (mappend (text " | ") . text . Text.unpack) ext <+> white rbrace
    where showField (k, v) = dullcyan (text $ Text.unpack k) <+> equals <+> pretty v
  pretty (VariantE n es)  = white langle <+> text (Text.unpack n) <+> hsep (pretty <$> es) <+> white rangle
  pretty (ProjE e)        = magenta (text "prj") <+> pretty e
  pretty (CaseE e bs)     = magenta (text "case") <+> pretty e <+> magenta (text "of") <+> encloseSep (white rbrace) (white rbrace) (white comma) (showBranch <$> bs)
    where showBranch (p, v) = pretty p <+> magenta (text "→") <+> pretty v

instance Pretty Pattern where
  pretty (IntP i)                = dullred $ integer i
  pretty (VarP v)                = white (text $ Text.unpack v)
  pretty WildcardP               = magenta $ text "_"
  pretty (ConstructorP v ps)     = parens $ dullgreen (text $ Text.unpack v) <+> hsep (pretty <$> ps)
  pretty (RecordP fields ignore) = white lbrace <+> mconcat (punctuate (white comma) (showField <$> fields)) <> (if ignore then text ", .." else empty) <+> white rbrace
    where showField (k, p) = dullcyan (text $ Text.unpack k) <+> equals <+> pretty p




type Subst = Map Text Type
type Env = Map Text Type

type MonadTC m = (MonadRWS Env [Constr] Integer m, MonadError Text m)

ftv :: Type -> Set Text
ftv (VarT v)           = Set.singleton v
ftv (ProductT ty)      = ftv ty
ftv (SumT ty)          = ftv ty
ftv (UnionT t1 t2)     = mconcat [ ftv t1, ftv t2 ]
ftv (ConstraintT cs t) = mconcat [ mconcat (ftv_c <$> cs), ftv t ]
ftv (ForallT vs t)     = ftv t Set.\\ Set.fromList vs
ftv (FunT t1 t2)       = mconcat [ ftv t1, ftv t2 ]
ftv (RowT fs)          = mconcat (ftv . snd <$> fs)
ftv _                  = mempty

ftv_c :: Constr -> Set Text
ftv_c (EqC t1 t2)     = mconcat [ ftv t1, ftv t2 ]
ftv_c (SubsetC t1 t2) = mconcat [ ftv t1, ftv t2 ]

applySubst :: Subst -> Type -> Type
applySubst s ty@(VarT v)        = Map.findWithDefault ty v s
applySubst s (ProductT ty)      = ProductT (applySubst s ty)
applySubst s (SumT ty)          = SumT (applySubst s ty)
applySubst s (UnionT t1 t2)     = UnionT (applySubst s t1) (applySubst s t2)
applySubst s (ConstraintT cs t) = ConstraintT (applySubst_s s <$> cs) (applySubst s t)
applySubst s (ForallT vs t)     = ForallT vs (applySubst newS t)
  where newS = Map.withoutKeys s $ Set.fromList vs
applySubst s (FunT t1 t2)       = FunT (applySubst s t1) (applySubst s t2)
applySubst s (RowT fs)          = RowT (second (applySubst s) <$> fs)
applySubst _ ty                 = ty

applySubst_s :: Subst -> Constr -> Constr
applySubst_s s (EqC t1 t2)     = EqC (applySubst s t1) (applySubst s t2)
applySubst_s s (SubsetC t1 t2) = SubsetC (applySubst s t1) (applySubst s t2)

freshVar :: MonadTC m => Text -> m Type
freshVar prefix = do
  n <- get
  put (n + 1)

  pure . VarT $ prefix <> Text.pack (show n)

runTypechecker :: Expr -> Either Text Type
runTypechecker ex = do
  trace ("-> Infering type of: " <> show (pretty ex)) $ pure ()

  (ty, constraints) <- runExcept $ evalRWST (typecheck ex) mempty 0

  trace ("--> Constraints to resolve: " <> show (pretty <$> constraints)) $ pure ()
  trace ("--> Type before constraint solving: " <> show (pretty ty)) $ pure ()

  (unsolved, _, sub) <-  loopUntil ([], constraints, mempty) \ (unsolved, constraints, subs) -> do
    (unsolved2, sub) <- runExcept $ resolveConstraints constraints

    trace ("--> Reducing unsolved constraints: " <> show (pretty <$> unsolved2)) $ pure ()

    -- try reducing unsolved constraints
    -- e.g.:
    -- - transitivity: a <: b, b <: c => a <: c
    -- - symetry: a <: b, b <: a => a = b
    (remaining, generated, sub2) <- runExcept $ reduceConstraints (unsolved <> unsolved2)
    trace ("--> Solving newly generated constraints: " <> show (pretty <$> generated)) $ pure ()

    pure (null generated, (remaining, generated, subs <> sub <> sub2))
  --(unsolved2, sub3) <- runExcept $ resolveConstraints generated

  trace ("--> Remaining constraints: " <> show (pretty <$> unsolved)) $ pure ()
  
  trace ("--> Complete substitution: " <> show (prettySub <$> Map.toList sub)) $ pure ()

  let generalizedTy = generalize mempty $ applySubst sub (ConstraintT unsolved ty)
  trace ("--> Type after constraint solving: " <> show (pretty generalizedTy)) $ pure ()

  pure generalizedTy
  where
    prettySub (v, t) = black (text $ Text.unpack v) <+> text "≡" <+> pretty t

    loopUntil input f = do
      (end, newInput) <- f input
      if end
      then pure newInput
      else loopUntil newInput f

reduceConstraints :: MonadError Text m => [Constr] -> m ([Constr], [Constr], Subst)
reduceConstraints []                   = pure ([], [], mempty)
reduceConstraints [c]                  = pure ([c], [], mempty)
reduceConstraints (SubsetC t1 t2 : cs) = do
  let (laws, left) = partition (subset t1 t2) cs
  (rem, gen, sub) <- reduceConstraints left

  let gen2 = (applySubst_s sub <$> laws) >>= \ case
        EqC _ _                   -> []
        -- symetry: t1 <: t2, t2 <: t1
        SubsetC t3 t4 | t1 == t4 && t2 == t3 -> [ EqC t1 t2 ]
        -- trans: t1 <: t2, t2 <: t3
        SubsetC t3 t4 | t1 == t4 || t2 == t3 ->
          if t1 == t4 then [ SubsetC t3 t2 ] else [ SubsetC t1 t4 ]
  
  pure (rem, gen2 <> gen, sub)
  where subset _ _ (EqC _ _)         = False
        subset t1 t2 (SubsetC t3 t4) = t1 == t4 || t2 == t3
reduceConstraints (c : cs)             = do
  (rem, gen, sub) <- reduceConstraints cs
  pure (applySubst_s sub c : rem, gen, sub)

resolveConstraints :: MonadError Text m => [Constr] -> m ([Constr], Subst)
resolveConstraints []       = pure ([], mempty)
resolveConstraints (c : cs) = do
  (cs1, sub1) <- case c of
    EqC t1 t2     -> ([],) <$> solveEq t1 t2
    SubsetC t1 t2 -> solveSubset t1 t2
  (cs2, sub2) <- resolveConstraints (applySubst_s sub1 <$> cs)

  let subs = sub1 <> sub2
  let css  = applySubst_s sub2 <$> (cs1 <> cs2)
      
  if hasConcreteConstraints css
  then second (mappend subs) <$> resolveConstraints css
  else pure (css, subs)

hasConcreteConstraints :: [Constr] -> Bool
hasConcreteConstraints [] = False
hasConcreteConstraints (c : cs) =
  hasConcreteConstraints cs || case c of
    EqC t1 t2     -> not (hasVar t1 && hasVar t2)
    SubsetC t1 t2 -> not (hasVar t1 && hasVar t2)

hasVar :: Type -> Bool
hasVar (VarT _)          = True
hasVar (RigidT _)        = False
hasVar (ProductT t)      = hasVar t
hasVar (SumT t)          = hasVar t
hasVar (UnionT t1 t2)    = hasVar t1 || hasVar t2
hasVar (ConstraintT _ t) = hasVar t
hasVar (ForallT _ t)     = hasVar t
hasVar (FunT t1 t2)      = hasVar t1 || hasVar t2
hasVar (RowT ts)         = any (hasVar . snd) ts
hasVar IntT              = False

generalize :: Env -> Type -> Type
generalize _ ty = ForallT tvars (rigidify ty)
  where tvars = Set.toList $ ftv ty

rigidify :: Type -> Type
rigidify (VarT v)           = RigidT v
rigidify (ProductT t)       = ProductT (rigidify t)
rigidify (SumT t)           = SumT (rigidify t)
rigidify (UnionT t1 t2)     = UnionT (rigidify t1) (rigidify t2)
rigidify (ConstraintT cs t) = ConstraintT (rigidify_c <$> cs) (rigidify t)
rigidify (ForallT vs t)     = ForallT newVars (rigidify t)
  where newVars             = Set.toList $ Set.fromList vs <> ftv t
rigidify (FunT t1 t2)       = FunT (rigidify t1) (rigidify t2)
rigidify (RowT ts)          = RowT (second rigidify <$> ts)
rigidify IntT               = IntT
rigidify (RigidT v)         = RigidT v

rigidify_c :: Constr -> Constr
rigidify_c (EqC t1 t2)     = EqC (rigidify t1) (rigidify t2)
rigidify_c (SubsetC t1 t2) = SubsetC (rigidify t1) (rigidify t2)

solveEq :: MonadError Text m => Type -> Type -> m Subst
solveEq t1 t2 = trace ("---> Applying constraint: " <> show (pretty $ EqC t1 t2)) $ case (t1, t2) of
  (_, _) | t1 == t2          -> pure mempty
  (VarT v, t2)               -> bind v t2
  (t1, VarT v)               -> bind v t1
  (ProductT z1, ProductT z2) -> solveEq z1 z2
  (_, _)                     ->
    throwError $ "Cannot unify type '" <> Text.pack (show (pretty t1)) <> "' with type '" <> Text.pack (show (pretty t2)) <> "'"

bind :: MonadError Text m => Text -> Type -> m Subst
bind v t | t == VarT v    = pure mempty
         | occurCheck v t = throwError $ "Cannot construct the infinite type '" <> v <> " ~ " <> Text.pack (show (pretty t)) <> "'"
         | otherwise      = pure $ Map.singleton v t
 where occurCheck v t = v `Set.member` ftv t

solveSubset :: MonadError Text m => Type -> Type -> m ([Constr], Subst)
solveSubset t1 t2 = trace ("---> Applying constraint: " <> show (pretty $ SubsetC t1 t2)) $ case (t1, t2) of
  (_, _) | hasVar t1 || hasVar t2 -> pure ([SubsetC t1 t2], mempty)
  (_, _)                          ->
    throwError $ "Cannot check subset relation on types '" <> Text.pack (show (pretty t1)) <> "' and '" <> Text.pack (show (pretty t2)) <> "'"


typecheck :: MonadTC m => Expr -> m Type
typecheck (IntE _)      = pure IntT
typecheck (VarE v)      = maybe (throwError $ "Unbound variable '" <> v <> "'") pure =<< asks (Map.lookup v)
typecheck (LamE v e)    = do
  t <- freshVar "t"
  rt <- local (Map.insert v t) (typecheck e)
  pure $ FunT t rt
typecheck (SelectE e v) = do
  t <- freshVar "t"
  z <- freshVar "z"
  zt <- typecheck e

  tell [EqC (ProductT z) zt, SubsetC (RowT [(v, t)]) z]

  pure t
typecheck (UnionE e1 e2) = do
  z1 <- freshVar "z"
  z2 <- freshVar "z"

  z1t <- typecheck e1
  z2t <- typecheck e2

  tell [EqC (ProductT z1) z1t, EqC (ProductT z2) z2t]

  pure (ProductT $ UnionT z1 z2)
typecheck (RecordE fs ext) = do
  tfs <- RowT <$> traverse (\ (n, v) -> (n,) <$> typecheck v) fs

  case ext of
    Nothing -> pure (ProductT tfs)
    Just v  -> do
      z <- freshVar "z"
      tv <- typecheck (VarE v)

      tell [EqC (ProductT z) tv]

      pure (ProductT $ UnionT tfs z)
typecheck (ProjE r) = do
  z1 <- freshVar "z"
  z2 <- freshVar "z"

  tr <- typecheck r

  tell [EqC (ProductT z1) tr, SubsetC z2 z1]

  pure (ProductT z2)
typecheck ex = throwError $ "typecheck '" <> Text.pack (show (pretty ex)) <> "': not yet implemented"
