\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module StrExpr where

class StrExpr expr where
    emptyS :: StrExpr
\end{code}