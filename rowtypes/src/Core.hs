module Core where

import Data.Set (Set)

data Expr
    = EInt Integer
    | EFloat Double
    | EChar Char
    | EId String
    | ELam String Expr
    | EApp Expr Expr
  deriving (Eq, Show)

data Type
    = TInt
    | TFloat
    | TChar
    | TArrow Type Type
    | TRigid String
    | TVar String
  deriving (Eq, Show)

data Scheme = Forall (Set String) Type
