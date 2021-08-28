{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module TC where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.RWS (MonadRWS, runRWST, gets, put, asks, local, tell, mfix)
import Control.Monad.Except (MonadError, runExcept, throwError)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable (fold)
import Debug.Trace (trace)
import Data.Function (fix)
import Data.List (nub)

data Expr
  = SelectE Expr Text
  | LamE Text Expr
  | VarE Text
  | IntE Integer
  | PlusE Expr Expr
  | AppE Expr Expr
  | RecordE (Map Text Expr) (Maybe Expr)

data Type
  = IntT
  | FunT Type Type
  | ProductT Type
  | RowT (Map Text Type) (Maybe Type)
  | ForallT [Text] Type
  | VarT Text
  | RigidT Text
  | ConstraintT [Constr] Type
  deriving (Eq)

data Constr
  = EqC Type Type
  deriving (Eq)

instance Pretty Expr where
  pretty (SelectE e x)  = maybeParensExpr e <> dot <> dullcyan (text $ Text.unpack x)
  pretty (LamE x e)     = magenta "λ" <+> dullblue (text $ Text.unpack x) <> dot <+> pretty e
  pretty (VarE v)       = dullblue (text $ Text.unpack v)
  pretty (IntE i)       = dullred $ integer i
  pretty (PlusE e1 e2)  = pretty e1 <+> yellow "+" <+> pretty e2
  pretty (AppE e1 e2)   = maybeParensExpr e1 <+> pretty e2
  pretty (RecordE fs e) = white lbrace <+> hcat (punctuate (white comma <> space) (prettyField <$> Map.toList fs)) <> maybe empty (mappend " | " . pretty) e <+> white rbrace
    where prettyField (x, e) = dullcyan (text $ Text.unpack x) <+> white equals <+> pretty e

maybeParensExpr :: Expr -> Doc
maybeParensExpr e@(PlusE _ _) = parens $ pretty e
maybeParensExpr e@(LamE _ _)  = parens $ pretty e
maybeParensExpr e@(AppE _ _)  = parens $ pretty e
maybeParensExpr e             = pretty e

instance Pretty Type where
  pretty IntT               = magenta "int"
  pretty (FunT t1 t2)       = maybeParensType t1 <+> yellow "→" <+> pretty t2
  pretty (ProductT t)       = magenta "∏" <> maybeParensType t
  pretty (RowT fs ext)      = white lbrace <+> hcat (punctuate (white comma <> space) $ prettyField <$> Map.toList fs) <> maybe empty (mappend " | " . pretty) ext <+> white rbrace
    where prettyField (x, e) = dullcyan (text $ Text.unpack x) <+> white (colon <> colon) <+> pretty e
  pretty (ForallT vs t)     = (if null vs then empty else magenta "∀" <+> hsep (green . text . Text.unpack <$> vs) <> white dot <> space) <> pretty t
  pretty (VarT v)           = black (text $ Text.unpack v)
  pretty (RigidT v)         = green (text $ Text.unpack v)
  pretty (ConstraintT cs t) = (if null cs then empty else white lparen <> hcat (punctuate (white comma <> space) $ pretty <$> cs) <> white rparen <+> yellow "⇒" <> space) <> pretty t

maybeParensType :: Type -> Doc
maybeParensType t@(ForallT _ _) = parens $ pretty t
maybeParensType t@(FunT _ _) = parens $ pretty t
maybeParensType t = pretty t

instance Pretty Constr where
  pretty (EqC t1 t2) = maybeParensType t1 <+> yellow "~" <+> maybeParensType t2



type Env = Map Text Type

type MonadTC m = (MonadRWS Env [Constr] Integer m, MonadError Text m)

type Subst = Map Text Type

(<·>) :: Subst -> Subst -> Subst
s1 <·> s2 = fmap (s1 ·>) s2 <> fmap (s2 ·>) s1

class Substitutable a where
  (·>) :: Subst -> a -> a

instance Substitutable a => Substitutable [a] where
  s ·> l = (·>) s <$> l

instance Substitutable Type where
  s ·> t@(VarT v)      = Map.findWithDefault t v s
  s ·> FunT t1 t2      = FunT (s ·> t1) (s ·> t2)
  s ·> ProductT t      = ProductT (s ·> t)
  s ·> RowT fs e       = RowT ((·>) s <$> fs) ((·>) s <$> e)
  s ·> ConstraintT c t = ConstraintT ((·>) s <$> c) (s ·> t)
  s ·> ForallT v t     = ForallT v (s1 ·> t)
    where s1           = Map.withoutKeys s (Set.fromList v)
  s ·> t               = t

instance Substitutable Constr where
  s ·> EqC t1 t2 = EqC (s ·> t1) (s ·> t2)

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  s ·> (x, y) = (s ·> x, s ·> y)

class Free t where
  ftv :: t -> Set Text

instance Free a => Free [a] where
  ftv l = fold $ ftv <$> l

instance Free Type where
  ftv (VarT v)           = Set.singleton v
  ftv (FunT t1 t2)       = mconcat [ ftv t1, ftv t2 ]
  ftv (ProductT t)       = ftv t
  ftv (RowT fs e)        = mconcat [ ftv $ Map.elems fs, maybe mempty ftv e ]
  ftv (ConstraintT cs t) = mconcat [ ftv cs, ftv t ]
  ftv (ForallT _ t)      = ftv t
  ftv _                  = mempty

instance Free Constr where
  ftv (EqC t1 t2) = mconcat [ ftv t1, ftv t2 ]

-----------------------------------------------------------------

rigidify :: Type -> Type
rigidify (VarT v)              = RigidT v
rigidify (FunT t1 t2)          = FunT (rigidify t1) (rigidify t2)
rigidify (ProductT t)          = ProductT (rigidify t)
rigidify (RowT fs e)           = RowT (rigidify <$> fs) (rigidify <$> e)
rigidify (ConstraintT cs t)    = ConstraintT (rigidify_c <$> cs) (rigidify t)
  where rigidify_c (EqC t1 t2) = EqC (rigidify t1) (rigidify t2)
rigidify (ForallT vs t)        = ForallT (Set.toList $ Set.fromList vs <> ftv t) (rigidify t)
rigidify t                     = t

generalize :: Env -> Type -> Type
generalize env ty = ForallT tvars (rigidify ty)
  where tvars = Set.toList $ ftv ty Set.\\ Map.keysSet env

simplify :: Type -> Type
simplify (ProductT t)                    = ProductT (simplify t)
simplify (RowT fs1 (Just (RowT fs2 ex))) = simplify (RowT (simplify <$> (fs1 <> fs2)) ex)
simplify (RowT fs ex)                    = RowT (simplify <$> fs) (simplify <$> ex)
simplify (FunT t1 t2)                    = FunT (simplify t1) (simplify t2)
simplify (ForallT v1 (ForallT v2 t))     = simplify (ForallT (nub $ v1 <> v2) t)
simplify (ForallT v t)                   = ForallT v (simplify t)
simplify (ConstraintT cs t)              = ConstraintT (simplify_c <$> cs) (simplify t)
  where simplify_c (EqC t1 t2) = EqC (simplify t1) (simplify t1)
simplify t                               = t

freshVar :: MonadTC m => Text -> m Type
freshVar prefix = do
  n <- gets (+ 1)
  put n

  pure $ VarT $ prefix <> Text.map toSubscript (Text.pack $ show n)
  where
    toSubscript '0' = '₀'
    toSubscript '1' = '₁'
    toSubscript '2' = '₂'
    toSubscript '3' = '₃'
    toSubscript '4' = '₄'
    toSubscript '5' = '₅'
    toSubscript '6' = '₆'
    toSubscript '7' = '₇'
    toSubscript '8' = '₈'
    toSubscript '9' = '₉'
    toSubscript c   = c

runTypechecker :: Expr -> Either Text Type
runTypechecker ex = do
  trace ("-> Infering type of: " <> show (pretty ex)) $ pure ()

  (ty, cnt, csts) <- runExcept $ runRWST (typecheck ex) mempty 0

  trace ("-> Infered type: " <> show (pretty ty)) $ pure ()
  trace ("-> Generated constraints: " <> show (pretty <$> csts)) $ pure ()

  (sub, rem) <- flip (loopWhile (null . snd)) (mempty, csts)
                \ (sub1, csts) -> do
                  (sub2, _, rem) <- runExcept $ runRWST (solveConstraints csts) mempty cnt
                  pure (sub1 <·> sub2, rem)

  trace ("-> Remaining constraints: " <> show (pretty <$> rem)) $ pure ()
  trace ("-> Final substitution: " <> show (showSubst <$> Map.toList sub)) $ pure ()

  pure $ simplify $ generalize mempty $ ConstraintT rem $ sub ·> ty
  where
    showSubst (v, t) = black (text $ Text.unpack v) <+> yellow "≡" <+> pretty t

    loopWhile pred f input =
      if pred input
      then pure input
      else loopWhile pred f =<< f input

solveConstraints :: MonadTC m => [Constr] -> m Subst
solveConstraints []               = pure mempty
solveConstraints (EqC t1 t2 : cs) = do
  sub1 <- solveEq t1 t2
  sub2 <- solveConstraints (sub1 ·> cs)
  pure (sub1 <·> sub2)

solveEq :: MonadTC m => Type -> Type -> m Subst
solveEq t1 t2 = trace ("--> Solving constraint: " <> show (pretty $ EqC t1 t2)) $ case (t1, t2) of
  _ | t1 == t2               -> pure mempty
  (VarT v, t2)               -> bind v t2
  (t1, VarT v)               -> bind v t1
  (ProductT t1, ProductT t2) -> solveEq t1 t2
  (RowT fs1 e1, RowT fs2 e2) -> do
    let commonFields = Map.intersectionWith (,) fs1 fs2
        extraFields1 = Map.difference fs2 fs1
        extraFields2 = Map.difference fs1 fs2

    sub1 <- solveEqMany $ Map.elems commonFields
    sub2 <- unifyRow t1 extraFields1 e1
    sub3 <- unifyRow t2 extraFields2 e2
    
    pure $ sub1 <·> sub2 <·> sub3
  (FunT t1 t2, FunT t3 t4)   -> solveEqMany [ (t1, t3), (t2, t4) ]
  (t1, t2)                   -> throwError $ "Cannot unify type '" <> Text.pack (show $ pretty t1) <> "' with type '" <> Text.pack (show $ pretty t2) <> "'"
  where
    unifyRow ty fs Nothing
      | Map.null fs        = pure mempty
      | otherwise          = throwError $ "Row '" <> Text.pack (show $ pretty ty) <> "' is missing fields " <> Text.intercalate ", " (Map.keys fs)
    unifyRow _ fs (Just e) = do
      ez <- freshVar "z"
      solveEq e (RowT fs $ Just ez)

solveEqMany :: MonadTC m => [(Type, Type)] -> m Subst
solveEqMany []              = pure mempty
solveEqMany ((t1, t2) : ts) = do
  sub1 <- solveEq t1 t2
  (<·>) sub1 <$> solveEqMany (sub1 ·> ts)

bind :: MonadTC m => Text -> Type -> m Subst
bind v t | t == VarT v    = pure mempty
         | occurCheck v t = throwError $ "Cannot construct the infinite type '" <> Text.pack (show $ pretty $ EqC (VarT v) t) <> "'"
         | otherwise      = pure $ Map.singleton v t

occurCheck :: Text -> Type -> Bool
occurCheck v t = v `Set.member` ftv t

typecheck :: MonadTC m => Expr -> m Type
typecheck (VarE v) = maybe (throwError $ "Unbound variable '" <> v <> "'") pure =<< asks (Map.lookup v)
typecheck (LamE v e) = do
  t1 <- freshVar "a"
  te <- local (Map.insert v t1) (typecheck e)

  pure $ FunT t1 te
typecheck (IntE _) = pure IntT
typecheck (PlusE e1 e2) = do
  te1 <- typecheck e1
  te2 <- typecheck e2

  tell [ EqC te1 IntT, EqC te2 IntT ]

  pure IntT
typecheck (SelectE e l) = do
  t1 <- freshVar "a"
  z2 <- freshVar "z"
  z3 <- freshVar "z"

  te <- typecheck e

  tell [ EqC te (ProductT z3), EqC z3 (RowT (Map.fromList [ (l, t1) ]) (Just z2)) ]

  pure t1
typecheck (AppE e1 e2) = do
  tr <- freshVar "a"

  te1 <- typecheck e1
  te2 <- typecheck e2

  tell [ EqC te1 (FunT te2 tr) ]

  pure tr
typecheck (RecordE fs ext) = do
  tfs <- traverse typecheck fs
  te <- traverse typecheck ext

  pure $ ProductT $ RowT tfs te
  
--typecheck e = throwError $ "typecheck '" <> Text.pack (show $ pretty e) <> "': not yet implemented"
