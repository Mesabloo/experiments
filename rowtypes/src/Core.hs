module Core where

import Data.Set (Set)
import Data.Map (Map)

data Expr
    = EInt Integer
    | EFloat Double
    | EChar Char
    | EId String
    | ELam String Expr
    | EApp Expr Expr
    | ERecord (Map String Expr)
  deriving (Eq, Show)

data Type
    = TInt
    | TFloat
    | TChar
    | TArrow Type Type
    | TRigid String
    | TVar String
    | TRecord Type
    | TRow (Map String Type) (Maybe Type)
  deriving (Eq, Show)

data Scheme = Forall (Set String) Type
