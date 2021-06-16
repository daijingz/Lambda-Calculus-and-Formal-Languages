\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module IntExpression where

import Data.List (intersperse)

intToDouble :: Int -> Double
intToDouble = fromIntegral

intToFloat :: Int -> Float
intToFloat = fromIntegral

floatToDouble :: Float -> Double
floatToDouble = realToFrac

doubleToFloat :: Double -> Float
doubleToFloat = realToFrac

class IntExpr expr where
  valI :: Int -> expr
  varI :: String -> expr
  addI :: expr -> expr -> expr
  subI :: expr -> expr -> expr
  mulI :: expr -> expr -> expr
  divI :: expr -> expr -> expr
  expI :: expr -> expr -> expr
  modI :: expr -> expr -> expr
  radianI :: Double -> expr
  degreeI :: Double -> expr
  sinI :: Double -> expr
  cosI :: Double -> expr
  tanI :: Double -> expr
  mtaddI :: [expr] -> expr
  mtmulI :: [expr] -> expr

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10 :: IntExpr expr => expr
ex1 = valI 10
ex2 = varI "X"
ex3 = addI (valI 10) (subI (valI 10) (valI 5))
ex4 = radianI pi
ex5 = mulI (valI 10) (varI "X")
ex6 = divI (valI 10) (varI "X")
ex7 = expI (valI 10) (modI (valI 10) (valI 3))
ex8 = mtaddI [sinI pi]
ex9 = mtmulI [cosI pi]
ex10 = tanI pi
\end{code}

\begin{code}
newtype Or = Or { value :: Double }

instance IntExpr Or where
  valI x = Or { value = intToDouble x }
  varI _ = Or { value = 0.0 }
  addI x y = Or { value = value x + value y }
  subI x y = Or { value = value x - value y }
  mulI x y = Or { value = value x * value y }
  divI x y = Or { value = value x / value y }
\end{code}

\begin{code}
newtype Pr = Pr { view :: String }

instance IntExpr Pr where
  valI x = Pr { view = show x }
  varI x = Pr { view = "var " ++ show x }
  addI x y = Pr { view = "add " ++ view x ++ " " ++ view y }
  subI x y = Pr { view = "sub " ++ view x ++ " " ++ view y }
  mulI x y = Pr { view = "mul " ++ view x ++ " " ++ view y }
  divI x y = Pr { view = "div " ++ view x ++ " " ++ view y }
  expI x y = Pr { view = "exp " ++ view x ++ " " ++ view y }
  modI x y = Pr { view = "mod " ++ view x ++ " " ++ view y }
  radianI x = Pr { view = "radian " ++ show x }
  degreeI x = Pr { view = "degree " ++ show x }
  sinI x = Pr { view = "sin " ++ show x }
  cosI x = Pr { view = "cos " ++ show x }
  tanI x = Pr { view = "tan " ++ show x }
  mtaddI [] = Pr { view = "(mtadd [" ++ "])" }
  mtaddI [x] = Pr { view = "(mtadd [" ++ view x ++ "])" }
  mtaddI x = Pr { view = "(mtadd [" ++ concat (intersperse " " (map view x)) ++ "])" }
  mtmulI [] = Pr { view = "(mtmul [" ++ "])" }
  mtmulI [x] = Pr { view = "(mtmul [" ++ view x ++ "])" }
  mtmulI x = Pr { view = "(mtmul [" ++ concat (intersperse " " (map view x)) ++ "])" }
\end{code}