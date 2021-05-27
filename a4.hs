{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module A4 where

-- The following are useful for the assignment
import Data.List (intersperse, nub)

-- Useful extra imports for the Bonus only
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader

-- Here is the class that the assignment is all about: a representation
-- of boolean expressions with variables.
-- Note: 
-- 1) true is represented as 'andB []' 
-- 2) false as 'orB []'
-- 3) 'andB [e]' means the same as 'e', same with 'orB [e]'.
class BoolExpr repr where
  varB :: String -> repr
  notB :: repr -> repr
  andB :: [repr] -> repr
  orB :: [repr] -> repr
  impliesB :: repr -> repr -> repr
  xorB :: [repr] -> repr

-- Some useful test cases:
ex1, ex2, ex3, ex4, ex5 :: BoolExpr repr => repr
ex1 = andB [andB [], notB $ varB "x", orB [varB "y", varB "z"]]
ex2 = andB [andB [], notB $ varB "x", orB [varB "y", varB "z", varB "x"]]
ex3 = andB [andB [orB [varB "w"]]]
ex4 = andB [varB "s"]
ex5 = andB []
-- You should add more test cases, enough to cover all the 'corner' cases,
-- and then run them through all your examples.

-- Zeroeth Question: why is it justified for andB, orB and xorB to take lists
-- as input?  What does |xorB []| mean?

-- Reason 1: All of them have 2 or more same-type expressions, and it is convenient to store it in a list
-- Reason 2: All of those inner boolean expressions are ordered and list is an ordered data type
-- Reason 3: Storing in list makes its parameters good to express and extract
-- Reason 4: Those objects with Empty list can represent default boolean values (True and False)

-- Meaning of |xorB []|: False.

----------------------------------------------------------------------------
-- First interpretation: as a String.
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

-- Test case:
-- view ex2 should return the String
-- "(andB [(andB []),(notB var \"x\"),(orB [var \"y\",var \"z\",var \"x\"])])"
-- so that 'putStrLn $ view ex2' gives
-- (andB [(andB []),(notB var "x"),(orB [var "y",var "z",var "x"])])
-- Note that 'view' should not do any simplifications; even though ex3
-- and ex4 mean "the same", they will print differently.

----------------------------------------------------------------------------
-- Second interpretation: pulling out the (free) variables:
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
-- Test case:
-- fv ex2 should return exactly
-- [ "x", "y", "z" ]
-- Hint: Data.List.nub

----------------------------------------------------------------------------
-- Third interpretation: as 'syntax'
data BE = Var String | Not BE | And BE BE | Or BE BE | TrueB | FalseB
  deriving Show
asBE :: BE -> BE
asBE = id

-- Hint: this instance has more cases than the above
-- Hint: foldr1
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

-- Test cases:
-- asBE ex1
-- And TrueB (And (Not (Var "x")) (Or (Var "y") (Var "z")))
-- asBE ex2
-- And TrueB (And (Not (Var "x")) (Or (Var "y") (Or (Var "z") (Var "x"))))
-- asBE ex3
-- Var "w"

----------------------------------------------------------------------------
-- Fourth question: the other direction!
toBoolExpr :: BoolExpr repr => BE -> repr
toBoolExpr (Var x) = varB x
toBoolExpr (Not x) = notB (toBoolExpr x)
toBoolExpr (And x (Var y)) = andB ([toBoolExpr x] ++ [varB y])
toBoolExpr (And x (And y z)) = andB ([toBoolExpr x] ++ [toBoolExpr y] ++ [toBoolExpr z])
toBoolExpr (And x y) = andB ([toBoolExpr x] ++ [toBoolExpr y])
toBoolExpr (Or x (Var y)) = orB ([toBoolExpr x] ++ [varB y])
toBoolExpr (Or x (Or y z)) = orB ([toBoolExpr x] ++ [toBoolExpr y] ++ [toBoolExpr z])
toBoolExpr (Or x y) = orB ([toBoolExpr x] ++ [toBoolExpr y])
toBoolExpr TrueB = andB []
toBoolExpr FalseB = orB []

ex1b, ex2b, ex3b, ex4b, ex5b, ex6b :: BE
ex1b = And TrueB (And (Not (Var "x")) (Or (Var "y") (Var "z")))
ex2b = And TrueB (And (Not (Var "x")) (Or (Var "y") (Or (Var "z") (Var "x"))))
ex3b = Var "w"
ex4b = Var "s"
ex5b = TrueB
ex6b = And TrueB (And (And (Var "a") (Var "b")) (Or (Var "c") (Var "d")))

-- Part of this question: give an example that shows that you can
-- go in both directions (between BR and (BoolExpr repr => repr))
-- but that the translation is _not_ the identity.
-- [i.e. add some code here, and then something in 'main' below
--  that clearly demonstrates it]
-- Hint: depth will, in general, change.

-- Answer: see line 145, 423 and 424.

----------------------------------------------------------------------------
-- Fifth question: compute the 'size' of an expression.
-- More precisely: every 'constructor' of the BoolExpr language counts
-- for 1.
-- size ex1 = 7
-- size ex2 = 8
-- size ex3 = 4
-- size ex4 = 2
newtype Size = Sz {size :: Int}

instance BoolExpr Size where
  varB _ = Sz{size = 1}
  notB x = Sz{size = 1 + size x}
  andB [] = Sz{size = 1}
  andB (x:xs) = Sz{size = size x + size (andB xs)}
  orB [] = Sz{size = 1}
  orB (x:xs) = Sz{size = size x + size (orB xs)}
  impliesB x y = Sz{size = size x + size y}
  xorB [] = Sz{size = 1}
  xorB (x:xs) = Sz{size = size x + size (xorB xs)}
----------------------------------------------------------------------------
-- Sixth question: compute the 'depth' of an expression (as a tree)
-- except that varB counts as 0 depth.
-- depth ex1 = 2
-- depth ex2 = 2
-- depth ex3 = 3
-- depth ex4 = 1
-- Hint: maximum
newtype Depth = De {depth :: Int}

instance BoolExpr Depth where
  varB _ = De{depth = 0}
  notB x = De{depth = 1 + depth x}
  andB [] = De{depth = 1}
  andB [x] = De{depth = 1 + depth x}
  andB x = De{depth = 1 + maximum (map depth x)}
  orB [] = De{depth = 1}
  orB [x] = De{depth = 1 + depth x}
  orB x = De{depth = 1 + maximum (map depth x)}
  impliesB x y = if depth x >= depth y
    then De{depth = 1 + depth x}
  else De{depth = 1 + depth y}
  xorB [] = De{depth = 1}
  xorB [x] = De{depth = 1 + depth x}
  xorB x = De{depth = 1 + maximum (map depth x)}
-- Lastly, give an explicit example where going to BE and then back
-- to repr changes the depth of the results.

------------------------------------------------------------------------
-- Bonus questions
--

-- Bonus 1: implement a (potentially failing) evaluator from
-- a Valuation (an assignment of Bool to variables).

-- Use the following definitions:
type Valuation = Map.Map String Bool

newtype NotFound = VarNotFound String

newtype Eval = Ev { val :: ExceptT NotFound (Reader Valuation) Bool}
  _ = Ev {}
-- Hint: ask, liftEither, sequence, Data.Foldable.and, and monads
instance BoolExpr Eval where

-- For the rest of the bonus questions, the 'tutorial' at
-- http://okmij.org/ftp/tagless-final/index.html#course-oxford
-- contains a *lot* of useful advice. The most relevant is in
-- section 2, but section 3 and 4 are full of fascinating stuff too!
--
-- Bonus 2: implement another "printer" like that of Pr but minimize
-- the number of () in the results. 

newtype Pr2 = Pr2 {view2 :: String}

instance BoolExpr Pr2 where
  varB x = Pr2 {view2 = "var " ++ show x}
  notB x = Pr2 {view2 = "notB $ " ++ view2 x}
  andB [] = Pr2 {view2 = "andB [" ++ "]"}
  andB [x] = Pr2 {view2 = "andB [" ++ view2 x ++ "]"}
  andB x = Pr2 {view2 = "andB [" ++ concat (intersperse " " (map view2 x)) ++ "]"}
  orB [] = Pr2 {view2 = "orB [" ++ "]"}
  orB [x] = Pr2 {view2 = "orB [" ++ view2 x ++ "]"}
  orB x = Pr2 {view2 = "orB [" ++ concat (intersperse " " (map view2 x)) ++ "]"}
  impliesB x y = Pr2 {view2 = "impliesB $" ++ view2 x ++ " $ " ++ view2 y}
  xorB [] = Pr2 {view2 = "xorB [" ++ "]"}
  xorB [x] = Pr2 {view2 = "xorB [" ++ view2 x ++ "]"}
  xorB x = Pr2 {view2 = "xorB [" ++ concat (intersperse " " (map view2 x)) ++ "]"}

-- Bonus 3: change BExpr so that it becomes possible to implement the CNOT
-- gate that is useful in quantum computing.
-- https://en.wikipedia.org/wiki/Controlled_NOT_gate

data BEqb = TrueQb | FalseQb deriving Show
data BEQB = Qubit BEqb BEqb

-- Bonus 4: implement an interpretation of BoolExpr that 
-- pushes all the 'notB' to be only on variables.
-- Hint: see push_neg in the tutorial.

-- Lecutre given function
data E = I Integer | N E | A E E
  deriving Show

-- invariants built in to the syntax!
data EE = F Factor |  AA EE EE
  deriving Show
data Factor = II Integer | NN Integer
  deriving Show

push_neg :: E -> EE
push_neg (I i)         = F $ II i
push_neg (N (I i))     = F $ NN i
push_neg (N (N x))     = push_neg x
push_neg (N (A i j))   = AA (push_neg (N i)) (push_neg (N j))
push_neg (A i j)       = AA (push_neg i) (push_neg j)


-- Bonus 5: convert a BoolExpr to an equivalent BoolExpr that is in
-- https://en.wikipedia.org/wiki/Conjunctive_normal_form

-- To run later parts without any errors, lease load this part first!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Because we need to have those data consturctors but we cannot modify class BoolExpr above
-- Because to make perfection and simplification on this question, there is NOOOOOO instance used
--data BoolExpr a = BAnd (BoolExpr a) (BoolExpr a)
--                | BOr  (BoolExpr a) (BoolExpr a)
--                | BNot (BoolExpr a)
--                | BTrue
--                | BFalse
--                | BConst a
--  deriving (Eq, Ord, Show)

cnf :: BE -> BE
cnf (Var x) = Var x
cnf (Not (And x y)) = Or (Not (cnf x)) (Not (cnf y))
cnf (Not (Or x y)) = And (Not (cnf x)) (Not (cnf y))
cnf (Not (Not x)) = cnf x
cnf (Not TrueB) = FalseB
cnf (Not FalseB) = TrueB
cnf (Not x) = Not (cnf x)
cnf (And (Or x y) z) = Or (And (cnf x) (cnf z)) (And (cnf y) (cnf z))
cnf (And x (Or y z)) = Or (And (cnf x) (cnf y)) (And (cnf x) (cnf z))
cnf (And x y) = And (cnf x) (cnf y)
cnf (Or (And x y) z) = And (Or (cnf x) (cnf z)) (Or (cnf y) (cnf z))
cnf (Or x (And y z)) = And (Or (cnf x) (cnf y)) (Or (cnf x) (cnf z))
cnf (Or x y) = Or (cnf x) (cnf y)
cnf TrueB = TrueB
cnf FalseB = FalseB

cnfbe :: BoolExpr -> BoolExpr
cnfbe (BConst a) = BConst a
cnfbe (BNot (BAnd x y)) = BOr (BNot (cnfbe x)) (BNot (cnfbe y))
cnfbe (BNot (BOr x y)) = BAnd (BNot (cnfbe x)) (BNot (cnfbe y))
cnfbe (BNot (BNot x)) = cnfbe x
cnfbe (BNot BTrue) = BFalse
cnfbe (BNot BFalse) = BTrue
cnfbe (BNot x) = BNot (cnfbe x)
cnfbe (BAnd (BOr x y) z) = BOr (BAnd (cnfbe x) (cnfbe z)) (BAnd (cnfbe y) (cnfbe z))
cnfbe (BAnd x (BOr y z)) = BOr (BAnd (cnfbe x) (cnfbe y)) (BAnd (cnfbe x) (cnfbe z))
cnfbe (BAnd x y) = BAnd (cnfbe x) (cnfbe y)
cnfbe (BOr (BAnd x y) z) = BAnd (BOr (cnfbe x) (cnfbe z)) (BOr (cnfbe y) (cnfbe z))
cnfbe (BOr x (BAnd y z)) = BAnd (BOr (cnfbe x) (cnfbe y)) (BOr (cnfbe x) (cnfbe z))
cnfbe (BOr x y) = BOr (cnfbe x) (cnfbe y)
cnfbe BTrue = BTrue
cnfbe BFalse = BFalse

-- Bonus 6: convert a BoolExpr to an equivalent BoolExpr that is in
-- https://en.wikipedia.org/wiki/Disjunctive_normal_form

dnf :: BE -> BE
dnf (Var x) = Var x
dnf (Not (And x y)) = Or (Not (dnf x)) (Not (dnf y))
dnf (Not (Or x y)) = And (Not (dnf x)) (Not (dnf y))
dnf (Not (Not x)) = dnf x
dnf (Not TrueB) = FalseB
dnf (Not FalseB) = TrueB
dnf (Not x) = Not (dnf x)
dnf (And (Or x y) z) = Or (And (dnf x) (dnf z)) (And (dnf y) (dnf z))
dnf (And x (Or y z)) = Or (And (dnf x) (dnf y)) (And (dnf x) (dnf z))
dnf (And x y) = And (dnf x) (dnf y)
dnf (Or (And x y) z) = And (Or (dnf x) (dnf z)) (Or (dnf y) (dnf z))
dnf (Or x (And y z)) = And (Or (dnf x) (dnf y)) (Or (dnf x) (dnf z))
dnf (Or x y) = Or (dnf x) (dnf y)
dnf TrueB = TrueB
dnf FalseB = FalseB

dnfbe :: BoolExpr -> BoolExpr
dnfbe (BConst a) = BConst a
dnfbe (BNot (BAnd x y)) = BOr (BNot (dnfbe x)) (BNot (dnfbe y))
dnfbe (BNot (BOr x y)) = BAnd (BNot (dnfbe x)) (BNot (dnfbe y))
dnfbe (BNot (BNot x)) = dnfbe x
dnfbe (BNot BTrue) = BFalse
dnfbe (BNot BFalse) = BTrue
dnfbe (BNot x) = BNot (dnfbe x)
dnfbe (BAnd (BOr x y) z) = BOr (BAnd (dnfbe x) (dnfbe z)) (BAnd (dnfbe y) (dnfbe z))
dnfbe (BAnd x (BOr y z)) = BOr (BAnd (dnfbe x) (dnfbe y)) (BAnd (dnfbe x) (dnfbe z))
dnfbe (BAnd x y) = BAnd (dnfbe x) (dnfbe y)
dnfbe (BOr (BAnd x y) z) = BAnd (BOr (dnfbe x) (dnfbe z)) (BOr (dnfbe y) (dnfbe z))
dnfbe (BOr x (BAnd y z)) = BAnd (BOr (dnfbe x) (dnfbe y)) (BOr (dnfbe x) (dnfbe z))
dnfbe (BOr x y) = BOr (dnfbe x) (dnfbe y)
dnfbe BTrue = BTrue
dnfbe BFalse = BFalse

-- Bonus 7: 'simplify' a BoolExpr.  Use the following logical
-- formulas to implement *generalized* simplifications
-- 1. true and x <-> x
-- 2. false or x <-> x
-- 3. x or x <-> x
-- 4. x and x <-> x
-- 5. false and y <-> false
-- 6. and & or are associative

simplifier :: BE -> BE
simplifier (Var x) = Var x
simplifier (Not (TrueB)) = FalseB
simplifier (Not (FalseB)) = TrueB
simplifier (Not (Not x)) = simplifier x
simplifier (Not x) = simplifier x
simplifier (And TrueB x) = simplifier x
simplifier (And x TrueB) = simplifier x
simplifier (And FalseB _) = FalseB
simplifier (And _ FalseB) = FalseB
simplifier (And (Var x) (Var y)) = if x == y
  then Var x
else And (Var x) (Var y)
simplifier (And x y) = And (simplifier x) (simplifier y)
simplifier (Or TrueB _) = TrueB
simplifier (Or _ TrueB) = TrueB
simplifier (Or FalseB x) = simplifier x
simplifier (Or x FalseB) = simplifier x
simplifier (Or (Var x) (Var y)) = if x == y
  then Var x
else Or (Var x) (Var y)
simplifier (Or x y) = Or (simplifier x) (simplifier y)
simplifier TrueB = TrueB
simplifier FalseB = FalseB

simplifierbe :: BoolExpr -> BoolExpr
simplifierbe (BConst x) = BConst x
simplifierbe (BNot (BTrue)) = BFalse
simplifierbe (BNot (BFalse)) = BTrue
simplifierbe (BNot (BNot x)) = simplifierbe x
simplifierbe (BNot x) = simplifierbe x
simplifierbe (BAnd BTrue x) = simplifierbe x
simplifierbe (BAnd x BTrue) = simplifierbe x
simplifierbe (BAnd BFalse _) = BFalse
simplifierbe (BAnd _ BFalse) = BFalse
simplifierbe (BAnd (BConst x) (BConst y)) = if x == y
  then BConst x
else BAnd (BConst x) (BConst y)
simplifierbe (BAnd x y) = BAnd (simplifierbe x) (simplifierbe y)
simplifierbe (BOr BTrue _) = BTrue
simplifierbe (BOr _ BTrue) = BTrue
simplifierbe (BOr BFalse x) = simplifierbe x
simplifierbe (BOr x BFalse) = simplifierbe x
simplifierbe (BOr (BConst x) (BConst y)) = if x == y
  then BConst x
else BOr (BConst x) (BConst y)
simplifierbe (BOr x y) = BOr (simplifierbe x) (simplifierbe y)
simplifierbe BTrue = BTrue
simplifierbe BFalse = BFalse

------------------------------------------------------------------------
-- You can hand in a filled-in A5.hs (i.e. named that), or A5.lhs 
-- or A5.org.

------------------------------------------------------------------------
main :: IO ()
main = do
  let exs  = [ex1, ex2, ex3, ex4, ex5]
  let exbs = [ex1b, ex2b, ex3b, ex4b, ex5b]
  putStrLn $ show $ map depth exs
  -- [2,2,3,1,1]
  putStrLn $ show $ map size exs
  -- [7,8,4,2,1]
  putStrLn $ show $ map view exs
  -- ["(andB [(andB []),(notB var \"x\"),(orB [var \"y\",var \"z\"])])","(andB [(andB []),(notB var \"x\"),(orB [var \"y\",var \"z\",var \"x\"])])","(andB [(andB [(orB [var \"w\"])])])","(andB [var \"s\"])","(andB [])"]

  putStrLn $ show $ map fv exs
  -- [["x","y","z"],["x","y","z"],["w"],["s"],[]]
  putStrLn $ show $ map asBE exs
  -- [And TrueB (And (Not (Var "x")) (Or (Var "y") (Var "z"))),And TrueB (And (Not (Var "x")) (Or (Var "y") (Or (Var "z") (Var "x")))),Var "w",Var "s",TrueB]
  putStrLn $ show $ map view $ map toBoolExpr exbs
  -- ["(andB [(andB []),(andB [(notB var \"x\"),(orB [var \"y\",var \"z\"])])])","(andB [(andB []),(andB [(notB var \"x\"),(orB [var \"y\",(orB [var \"z\",var \"x\"])])])])","var \"w\"","var \"s\"","(andB [])"]

  putStrLn $ show $ asBE (toBoolExpr (asBE (And TrueB (And (And (Var "a") (Var "b")) (Or (Var "c") (Var "d"))))))
  putStrLn $ show $ asBE $ And TrueB (And (And (Var "a") (Var "b")) (Or (Var "c") (Var "d")))