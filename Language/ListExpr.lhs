\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ListExpr where

class ListExpr expr where
    emptyL :: ListExpr
\end{code}