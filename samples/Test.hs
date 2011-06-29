module Main where
import MiniSat2

main :: IO ()
main = do
  solver <- newSolver
  a <- newVar solver
  b <- newVar solver

  addClause solver [Pos a, Pos b]
  addClause solver [Neg a, Pos b]
  addClause solver [Pos a, Neg b]

  r <- solve solver
  if r
    then do
      putStrLn "satisfiable"
      modelValue solver a >>= \v -> putStrLn ("a = " ++ show v)
      modelValue solver b >>= \v -> putStrLn ("b = " ++ show v)
    else
      putStrLn "unsatisfiable"
