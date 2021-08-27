{-# LANGUAGE OverloadedStrings #-}

module Main where
    
import TC
import qualified Data.Text.IO as Text
import Text.PrettyPrint.ANSI.Leijen (putDoc, pretty)

main :: IO ()
main = do
  let expr = LamE "m" $ LamE "n" $ SelectE (UnionE (VarE "m") (VarE "n")) "x"
  printTypeOrError expr
  -- λ m. λ n. (m ★ n).x :: ∀ t z₁ z₂. ({ x :: t } ⧀ z₁ ⊙ z₂) ⇒ ∏z₁ → ∏z₂ → t
  
  let expr = LamE "r" $ SelectE (VarE "r") "x"
  printTypeOrError expr
  -- λ r. r.x :: ∀ t z1. ({ x :: t } ⧀ z1) ⇒ ∏z₁ → t
  
  let expr = LamE "v" $ LamE "r" $ RecordE [("x", VarE "v")] (Just "r")
  printTypeOrError expr
  -- λ v. λ r. { x = v | r } :: ∀ t z. t → ∏z → ∏({x :: t} ⊙ z)

  let expr = LamE "r" $ ProjE (VarE "r")
  printTypeOrError expr
  -- λ r. prj r :: ∀ z₁ z₂. (z₂ ⧀ z₁) ⇒ ∏z₁ → ∏z₂

  let expr = LamE "m" $ LamE "n" $ UnionE (VarE "m") (VarE "n")
  printTypeOrError expr
  -- λ m. λ n. m ★ n :: ∀ z₁ z₂. ∏z₁ → ∏z₂ → ∏(z₁ ⊙ z₂)

  let expr = LamE "r" $ SelectE (ProjE (VarE "r")) "x"
  printTypeOrError expr
  -- λ r. (prj r).x :: ∀ t z₁. ({ x :: t } ⧀ z₁) ⇒ ∏z₁ → t

  let expr = LamE "r" $ PlusE (SelectE (VarE "r") "x") (IntE 3)
  printTypeOrError expr
  -- λ r. r.x + 3 :: ∀ z₁. ({ x :: int } ⧀ z₁) ⇒ ∏z₁ → int

  let expr = LamE "r" $ LamE "x" $ PlusE (SelectE (ProjE (VarE "r")) "y") (VarE "x")
  printTypeOrError expr
  -- λ r. λ x. (prj r).y + x :: ∀ z₁. ({ y :: int } ⧀ z₁) ⇒ ∏z₁ → int → int

  let expr = LamE "r" $ SelectE (SelectE (VarE "r") "x") "y"
  printTypeOrError expr
  -- λ r. r.x.y :: ∀ t z₁ z₂. ({x :: ∏z₂} ⧀ z₁, {y :: t} ⧀ z₂) ⇒ ∏z₁ → t

  pure ()
  where
    printTypeOrError ex =
      case runTypechecker ex of
        Left err -> Text.putStrLn $ "> error: " <> err
        Right ty -> do
          putStr "> "
          putDoc (pretty ex)
          putStrLn ""
          putStr ">   :: "
          putDoc (pretty ty)
          putStrLn "\n"
