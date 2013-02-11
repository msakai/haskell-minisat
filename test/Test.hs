-- {-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Test.HUnit hiding (Test)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import MiniSat2

-- should be SAT
case_test1 :: IO ()
case_test1 = do
  solver <- newSolver
  a <- newVar solver
  b <- newVar solver
  addClause solver [Pos a, Pos b]
  addClause solver [Neg a, Pos b]
  addClause solver [Pos a, Neg b]
  r <- solve solver
  r @?= True

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain [group]
-- $(defaultMainGenerator)

group = testGroup "SomeModule" [testCase "test1" case_test1]
