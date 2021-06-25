\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module NumExpr where

import Data.List (intersperse, nub)

intToDouble :: Int -> Double
intToDouble = fromIntegral

intToFloat :: Int -> Float
intToFloat = fromIntegral

floatToDouble :: Float -> Double
floatToDouble = realToFrac

doubleToFloat :: Double -> Float
doubleToFloat = realToFrac

class NumExpr expr where
  valI :: Int -> expr
  varI :: String -> expr
  addI :: expr -> expr -> expr
  subI :: expr -> expr -> expr
  mulI :: expr -> expr -> expr
  divI :: expr -> expr -> expr
  expI :: expr -> Int -> expr
  modI :: Int -> Int -> expr
  radianI :: Double -> expr
  degreeI :: Double -> expr
  sinI :: Double -> expr
  cosI :: Double -> expr
  tanI :: Double -> expr
  mtaddI :: [expr] -> expr
  mtmulI :: [expr] -> expr

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10 :: NumExpr expr => expr
ex1 = valI 10
ex2 = varI "X"
ex3 = addI (valI 10) (subI (valI 10) (valI 5))
ex4 = radianI pi
ex5 = mulI (valI 10) (varI "X")
ex6 = divI (valI 10) (varI "X")
ex7 = modI 10 3
ex8 = mtaddI [sinI pi]
ex9 = mtmulI [cosI pi]
ex10 = tanI pi
\end{code}

\begin{code}
newtype Or = Or { value :: Double }

instance NumExpr Or where
  valI x = Or { value = intToDouble x }
  varI _ = Or { value = 0.0 }
  addI x y = Or { value = value x + value y }
  subI x y = Or { value = value x - value y }
  mulI x y = Or { value = value x * value y }
  divI x y = Or { value = value x / value y }
  expI x y = Or { value = value x ^^ y }
  modI x y = Or { value = intToDouble (x `mod` y) }
  radianI x = Or { value = x }
  degreeI x = Or { value = (x / 360) * 2 * pi }
  sinI x = Or { value = sin x }
  cosI x = Or { value = cos x }
  tanI x = Or { value = tan x }
  mtaddI [] = Or { value = 0.0 }
  mtaddI [x] = Or { value = value x }
  mtaddI xs = Or { value = value (head xs) + value (mtaddI (tail xs)) }
  mtmulI [] = Or { value = 0.0 }
  mtmulI [x] = Or { value = value x }
  mtmulI xs = Or { value = value (head xs) * value (mtmulI (tail xs)) }
\end{code}

\begin{code}
newtype Pr = Pr { view :: String }

instance NumExpr Pr where
  valI x = Pr { view = show x }
  varI x = Pr { view = "var " ++ show x }
  addI x y = Pr { view = "add " ++ view x ++ " " ++ view y }
  subI x y = Pr { view = "sub " ++ view x ++ " " ++ view y }
  mulI x y = Pr { view = "mul " ++ view x ++ " " ++ view y }
  divI x y = Pr { view = "div " ++ view x ++ " " ++ view y }
  expI x y = Pr { view = "exp " ++ view x ++ " " ++ show y }
  modI x y = Pr { view = "mod " ++ show x ++ " " ++ show y }
  radianI x = Pr { view = "radian " ++ show x }
  degreeI x = Pr { view = "degree " ++ show x }
  sinI x = Pr { view = "sin " ++ show x }
  cosI x = Pr { view = "cos " ++ show x }
  tanI x = Pr { view = "tan " ++ show x }
  mtaddI [] = Pr { view = "(mtadd [" ++ "])" }
  mtaddI [x] = Pr { view = "(mtadd [" ++ view x ++ "])" }
  mtaddI xs = Pr { view = "(mtadd [" ++ concat (intersperse " " (map view xs)) ++ "])" }
  mtmulI [] = Pr { view = "(mtmul [" ++ "])" }
  mtmulI [x] = Pr { view = "(mtmul [" ++ view x ++ "])" }
  mtmulI xs = Pr { view = "(mtmul [" ++ concat (intersperse " " (map view xs)) ++ "])" }
\end{code}

\begin{code}
newtype FV = FV {fv :: [String]}

instance NumExpr FV where
  valI _ = FV { fv = [] }
  varI x = FV { fv = [show x] }
  addI x y = FV { fv = fv x ++ fv y }
  subI x y = FV { fv = fv x ++ fv y }
  mulI x y = FV { fv = fv x ++ fv y }
  divI x y = FV { fv = fv x ++ fv y }
  expI x _ = FV { fv = fv x }
  modI _ _ = FV { fv = [] }
  radianI _ = FV { fv = [] }
  degreeI _ = FV { fv = [] }
  sinI _ = FV { fv = [] }
  cosI _ = FV { fv = [] }
  tanI _ = FV { fv = [] }
  mtaddI [] = FV { fv = [] }
  mtaddI x = FV { fv = nub (fv (head x) ++ fv (mtaddI (tail x))) }
  mtmulI [] = FV { fv = [] }
  mtmulI x = FV { fv = nub (fv (head x) ++ fv (mtmulI (tail x))) }
\end{code}

\begin{code}
data NE = Var String | IVal Int | FVal Double | Add NE NE | Sub NE NE | Mul NE NE | Div NE NE | Exp NE NE | Mod NE NE | Sin NE | Cos NE | Tan NE 
  deriving (Show, Eq)

asNE :: NE -> NE
asNE = id

instance NumExpr NE where
  valI x = IVal x
  varI x = Var x
  addI x y = Add (asNE x) (asNE y)
  subI x y = Sub (asNE x) (asNE y)
  mulI x y = Mul (asNE x) (asNE y)
  divI x y = Div (asNE x) (asNE y)
  expI x y = Exp (asNE x) (IVal y)
  modI x y = Mod (IVal x) (IVal y)
  radianI x = FVal x
  degreeI x = FVal x
  sinI x = Sin (FVal x)
  cosI x = Cos (FVal x)
  tanI x = Tan (FVal x)
  mtaddI [] = IVal 0
  mtaddI (x:xs) = Add (asNE x) (asNE (mtaddI xs))
  mtmulI [] = IVal 0
  mtmulI (x:xs) = Mul (asNE x) (asNE (mtmulI xs))

eval :: NE -> Double
eval (Var _) = 0.0
eval (IVal x) = intToDouble x
eval (FVal x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x / eval y
eval (Exp x y) = eval x ^^ toInteger (round (eval y))

represent :: NE -> String
represent (Var x) = "Var " ++ show x
represent (IVal x) = "IVal " ++ show x
represent (FVal x) = "FVal " ++ show x

extract :: NE -> [String]
extract (Var x) = [x]
extract (IVal _) = []
extract (FVal _) = []
\end{code}