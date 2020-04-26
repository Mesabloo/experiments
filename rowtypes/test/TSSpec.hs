{-# LANGUAGE BlockArguments #-}

module TSSpec (spec) where

import Test.Hspec
import Core
import TypeSystem
import qualified Data.Map as Map

spec :: Spec
spec = parallel do
    describe ":t 0" do
        it "must be TInt" do
             fmap snd (runTypecheck (elabExpr (EInt 0)) mempty) `shouldBe` Right TInt
    describe ":t 'c'" do
        it "must be TChar" do
             fmap snd (runTypecheck (elabExpr (EChar 'c')) mempty) `shouldBe` Right TChar
    describe ":t 0.0" do
        it "must be TFloat" do
             fmap snd (runTypecheck (elabExpr (EFloat 0.0)) mempty) `shouldBe` Right TFloat
    describe ":t \\x -> x" do
        it "must be TArrow $0 $0" do
             fmap snd (runTypecheck (elabExpr (ELam "x" (EId "x"))) mempty) `shouldBe` Right (TArrow (TVar "$0") (TVar "$0"))
    describe ":t { x = 0 ; y = 'c'}" do
        it "must be TRecord [(\"x\", TInt), (\"y\", TChar)]" do
             fmap snd (runTypecheck (elabExpr (ERecord (Map.fromList [("x", EInt 0), ("y", EChar 'c')]))) mempty) `shouldBe` Right (TRecord (TRow (Map.fromList [("x", TInt), ("y", TChar)]) Nothing))
