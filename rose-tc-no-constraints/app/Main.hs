{-# LANGUAGE OverloadedStrings #-}

module Main where
    
import TC
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Map as Map

main :: IO ()
main = do
  let expr = LamE "x" $ PlusE (VarE "x") (IntE 1)
  printTypeOrError expr
  -- λ x. x + 1 :: int -> int

  let expr = LamE "r" $ SelectE (VarE "r") "x"
  printTypeOrError expr
  -- λ r. r.x :: ∀ t z. ∏{ x :: t | z } → t

  let expr = LamE "r" $ PlusE (SelectE (VarE "r") "x") (IntE 2)
  printTypeOrError expr
  -- λ r. r.x + 2 :: ∀ z. ∏{ x :: int | z } → int

  let expr = LamE "r" $ PlusE (SelectE (VarE "r") "x") (SelectE (VarE "r") "y")
  printTypeOrError expr
  -- λ r. r.x + r.y :: ∀ z. ∏{ x :: int, y :: int | z } → int

  let expr = LamE "r" $ SelectE (SelectE (VarE "r") "x") "y"
  printTypeOrError expr
  -- λ r. r.x.y :: ∀ t z₁ z₂. ∏{ x :: ∏{ y :: t | z₁ } | z₂ } → t

  let expr = LamE "f" $ LamE "r" $ AppE (VarE "f") (SelectE (VarE "r") "x")
  printTypeOrError expr
  -- λ f. λ r. f (r.x)

  let expr = LamE "r" $ AppE (LamE "x" $ VarE "x") (SelectE (VarE "r") "x")
  printTypeOrError expr
  -- λ r. (λ x. x) (r.x) :: ∀ t z. ∏{ x :: t | z } → t

  let expr = LamE "x" $ LamE "r" $ RecordE (Map.singleton "x" (VarE "x")) (Just $ VarE "r")
  printTypeOrError expr
  -- λ x. λ r. { x = x | r } :: ∀ t z. t → ∏z → ∏{ x :: t | z }

  let expr = LamE "x" $ LamE "r" $ RecordE (Map.singleton "x" (PlusE (VarE "x") (IntE 1))) (Just $ VarE "r")
  printTypeOrError expr
  -- λ x. λ r. { x = x + 1 | r } :: ∀ z. int → ∏z → ∏{ x :: int | z }

  pure ()


printTypeOrError :: Expr -> IO ()
printTypeOrError ex =
  case runTypechecker ex of
    Left err -> putDoc $ ">" <+> pretty ex <+> "=> error:" <+> text (Text.unpack err) <> hardline <> hardline
    Right ty -> putDoc $ ">" <+> pretty ex <+> colon <> colon <+> pretty ty <> hardline <> hardline
