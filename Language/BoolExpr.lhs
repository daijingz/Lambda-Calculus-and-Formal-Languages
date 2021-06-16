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
  existB :: [expr] -> expr
  forallB :: [expr] -> expr
\end{code}