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
            runTypecheck (inferExpr (EInt 0)) mempty `shouldBe` Right TInt
    describe ":t 'c'" do
        it "must be TChar" do
             runTypecheck (inferExpr (EChar 'c')) mempty `shouldBe` Right TChar
    describe ":t 0.0" do
        it "must be TFloat" do
             runTypecheck (inferExpr (EFloat 0.0)) mempty `shouldBe` Right TFloat
    describe ":t \\x -> x" do
        it "must be TArrow $0 $0" do
             runTypecheck (inferExpr (ELam "x" (EId "x"))) mempty `shouldBe` Right (TArrow (TVar "$0") (TVar "$0"))
