\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BoolExpr where

import Data.List (intersperse, nub)

class BoolExpr expr where
  varB :: String -> expr
  notB :: expr -> expr
  andB :: [expr] -> expr
  orB :: [expr] -> expr
  impliesB :: expr -> expr -> expr
  xorB :: [expr] -> expr
  containsB :: expr -> expr -> Bool
  lessB :: Int -> Int -> Bool
  greaterB :: Int -> Int -> Bool

ex1, ex2, ex3, ex4, ex5 :: BoolExpr expr => expr
ex1 = varB "X"
ex2 = notB (varB "X")
ex3 = andB [varB "X", notB (varB "Y")]
ex4 = orB [varB "X", notB (varB "Y")]
ex5 = impliesB (varB "X") (varB "X")
\end{code}

\begin{code}
newtype Pr = Pr {view :: String}

instance BoolExpr Pr where
  varB x = Pr {view = "var " ++ show x}
  notB x = Pr {view = "notB $ " ++ view x}
  andB [] = Pr {view = "(andB [" ++ "])"}
  andB [x] = Pr {view = "(andB [" ++ view x ++ "])"}
  andB x = Pr {view = "(andB [" ++ concat (intersperse " " (map view x)) ++ "])"}
  orB [] = Pr {view = "(orB [" ++ "])"}
  orB [x] = Pr {view = "(orB [" ++ view x ++ "])"}
  orB x = Pr {view = "(orB [" ++ concat (intersperse " " (map view x)) ++ "])"}
  impliesB x y = Pr {view = "impliesB $" ++ view x ++ " $ " ++ view y}
  xorB [] = Pr {view = "(xorB [" ++ "])"}
  xorB [x] = Pr {view = "(xorB [" ++ view x ++ "])"}
  xorB x = Pr {view = "(xorB [" ++ concat (intersperse " " (map view x)) ++ "])"}
\end{code}

\begin{code}
newtype FV = FV {fv :: [String]}

instance BoolExpr FV where
  varB x = FV {fv = [x]}
  notB x = FV {fv = fv x}
  andB [] = FV {fv = []}
  andB x = FV {fv = nub (fv (head x) ++ fv (andB (tail x)))}
  orB [] = FV {fv = []}
  orB x = FV {fv = nub(fv (head x) ++ fv (orB (tail x)))}
  impliesB x y = FV {fv = nub(fv x ++ fv y)}
  xorB [] = FV {fv = []}
  xorB x = FV {fv = nub(fv (head x) ++ fv (xorB (tail x)))}
\end{code}

\begin{code}
data BE = Var String | Not BE | And BE BE | Or BE BE | TrueB | FalseB
  deriving Show
asBE :: BE -> BE
asBE = id

instance BoolExpr BE where
  varB x = Var x
  notB x = Not (asBE x)
  andB [] = TrueB
  andB [x] = asBE x
  andB x = if length x == 2
    then And (asBE (head x)) (asBE (last x))
  else And (asBE (head x)) (asBE (andB (tail x)))
  orB [] = FalseB
  orB [x] = asBE x
  orB x = if length x == 2
    then Or (asBE (head x)) (asBE (last x))
  else Or (asBE (head x)) (asBE (orB (tail x)))
  impliesB b1 b2 = And (Not (asBE b1)) (asBE b2)
  xorB xs = foldr1 (\a b -> And (Or (Not a) b) (And a (Not b))) (map asBE xs)
\end{code}