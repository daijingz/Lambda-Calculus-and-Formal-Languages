\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module StrExpr where

class StrExpr expr where
    emptyS :: StrExpr
    lowerA :: StrExpr
    lowerB :: StrExpr
    lowerC :: StrExpr
    lowerD :: StrExpr
    lowerE :: StrExpr
    lowerF :: StrExpr
    lowerG :: StrExpr
    word :: [StrExpr] -> StrExpr
    phrase2 :: word -> emptyS -> word -> StrExpr
    phrase3 :: word -> emptyS -> word -> emptyS -> word -> StrExpr

ex1, ex2, ex3, ex4, ex5 :: StrExpr expr => expr
ex1 = empty
ex2 = [lowerA, lowerB, empty]
ex3 = lowerC
ex4 = [lowerA, lowerB, lowerC, lowerD, empty]
ex5 = lowerE
\end{code}