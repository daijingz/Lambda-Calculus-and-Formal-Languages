\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module IntExpression where
class IntExpr expr where
  valI :: Int -> expr
  varI :: String -> expr
  addI :: expr -> expr -> expr
  subI :: expr -> expr -> expr
  mulI :: expr -> expr -> expr
  divI :: expr -> expr -> expr
  expI :: expr -> expr -> expr
  modI :: expr -> expr -> expr
  piI :: Double -> expr
  sinI :: Double -> expr
  cosI :: Double -> expr
  tanI :: Double -> expr
  mtaddI :: [expr] -> expr
  mtmulI :: [expr] -> expr

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10 :: IntExpr expr => expr
ex1 = valI 10
ex2 = varI "X"
ex3 = addI (valI 10) (subI (valI 10) (valI 5))
ex4 = piI pi
ex5 = mulI (valI 10) (varI "X")
ex6 = divI (valI 10) (varI "X")
ex7 = expI (valI 10) (modI (valI 10) (valI 3))
ex8 = mtaddI [sinI pi]
ex9 = cosI pi
ex10 = tanI pi
\end{code}