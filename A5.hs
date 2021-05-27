{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module A5 where

import Prelude hiding ((>>), drop)

--import Data.Bifunctor ( Bifunctor(first) )

--import Language.Haskell.TH ( Q, TExp )
--import Language.Haskell.TH.Syntax (Lift)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.PrettyPrint (Doc, text, hsep, parens, (<+>))
import qualified Text.PrettyPrint as P


{------------------------------------------------------------------------------
-- Recalling our Forth-like stack-based language...

Take the StackMachine from tutorial 10, and augment it with just 
enough features to be able to implement the heart of FizzBuzz 

https://skilldrick.github.io/easyforth/ is a helpful resource here for
better understanding Forth.

Note that the JVM is a stack machine, as is postscript (the heart of PDF),
so this is like "assembler" for quite a few languages.
------------------------------------------------------------------------------}

-- The signature is completely given, to make things simpler:
class StackMachine stk where
    empty :: stk ()

    push :: Lift a => a -> stk s -> stk (a, s)
    drop :: stk (a, s) -> stk s

    swap :: stk (a, (b, s)) -> stk (b, (a, s))
    dup  :: stk (a, s) -> stk (a, (a, s))
    rot   :: stk (a, (b, (c, s))) -> stk (b, (c, (a, s)))
    rot23 :: stk (a, (b, (c, s))) -> stk (a, (c, (b, s)))

    -- 's' prefix to obvious things to avoid name clashes
    sadd :: Num a => stk (a, (a, s)) -> stk (a, s)
    ssub :: Num a => stk (a, (a, s)) -> stk (a, s)
    smul :: Num a => stk (a, (a, s)) -> stk (a, s)
    sleq :: Ord a => stk (a, (a, s)) -> stk (Bool, s)
    seql :: Eq  a => stk (a, (a, s)) -> stk (Bool, s)
    smod :: Integral a => stk (a, (a, s)) -> stk (a, s)
    sand :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    sor :: stk (Bool, (Bool, s)) -> stk (Bool, s)
    snot  :: stk (Bool, s) -> stk (Bool, s)

    sappend :: stk (String, (String, s)) -> stk (String, s)

    spair :: stk (a, (b, s)) -> stk ((a, b), s)
    unpair :: stk ((a, b), s) -> stk (a, (b, s))
    sfst  :: stk ((a, b), s) -> stk (a, s)
    ssnd  :: stk ((a, b), s) -> stk (b, s)

    skip :: stk s -> stk s

    ifThenElse :: stk (Bool, (a, (a, s))) -> stk (a, s)

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) = flip (.)

{------------------------------------------------------------------------------
-- Q1.a
------------------------------------------------------------------------------}

-- Write a program with the following signature that takes as input
-- an Int, and returns a Bool for if the input is 0 mod 3, and
-- the string " Fizz" if True, "" otherwise
fizz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
fizz = push " Fizz" 
    >> swap 
    >> push "" 
    >> swap 
    >> push 3 
    >> smod 
    >> push 0 
    >> sleq 
    >> ifThenElse 
    >> dup 
    >> push True 
    >> swap 
    >> push False 
    >> swap 
    >> push " Fizz" 
    >> seql 
    >> ifThenElse

-- Write a program with the following signature that takes as input
-- an Int, and returns a Bool for if the input is 0 mod 5, and
-- the string " Buzz" if True, "" otherwise
buzz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
buzz = push " Buzz" 
    >> swap 
    >> push "" 
    >> swap 
    >> push 5 
    >> smod 
    >> push 0 
    >> sleq 
    >> ifThenElse 
    >> dup 
    >> push True 
    >> swap 
    >> push False 
    >> swap 
    >> push " Buzz" 
    >> seql 
    >> ifThenElse

-- Write a program with the following signature that takes as input
-- an Int, and returns the following:
-- let (b1, s1) the return of calling fizz
-- let (b2, s2) the return of calling buzz
-- the output will be (not (b1 or b2), s1 ++ s2)
-- this involves a lot of stack manipulation!  My version of this code
-- is 14 instructions long (but I don't guarantee that's optimal)
fizzbuzz :: (StackMachine stk) => stk (Int, s) -> stk (Bool, (String, s))
fizzbuzz = dup
    >> push " Fizz" 
    >> swap 
    >> push "" 
    >> swap 
    >> push 3 
    >> smod 
    >> push 0 
    >> sleq 
    >> ifThenElse
    >> swap
    >> push " Buzz" 
    >> swap 
    >> push "" 
    >> swap 
    >> push 5 
    >> smod 
    >> push 0 
    >> sleq 
    >> ifThenElse
    >> sappend
    >> dup
    >> push True 
    >> swap 
    >> push False 
    >> swap 
    >> push " Fizz Buzz" 
    >> seql 
    >> ifThenElse

{------------------------------------------------------------------------------
-- Q1.b
------------------------------------------------------------------------------}

-- implement an instance of StackMachine for R.
-- The following lift combinators are useful.
-- (No Applicative or Monad instance, as all functions are unary, in a sense)
newtype R a = R {unR :: a}

instance Functor R where
  fmap f = R . f . unR

liftR1 :: (a -> b) -> R (a, s) -> R (b, s)
liftR1 f = R . (\(x,y) -> (f x , y)) . unR

liftR2 :: (a -> b -> c) -> R (a, (b, s)) -> R (c, s)
liftR2 f (R (a, (b, s))) = R (f a b, s)

instance StackMachine R where
  empty = R ()
  push x (R s) = R (x, s)
  drop (R (_, s)) = R s
  swap (R (x, (y, s))) = R (y, (x, s))
  dup (R (x, s)) = (R (x, (x, s)))
  rot (R (x, (y, (z, s)))) = R (y, (z, (x, s)))
  rot23 (R (x, (y, (z, s)))) = R (x, (z, (y, s)))
  sadd (R (x, (y, s))) = R (y + x, s)
  ssub (R (x, (y, s))) = R (y - x, s)
  smul (R (x, (y, s))) = R (x * y, s)
  smod (R (x, (y, s))) = R (y `mod` x, s)
  sleq (R (x, (y, s))) = R (y <= x, s)
  seql (R (x, (y, s))) = R (x == y, s)
  sand (R (x, (y, s))) = R (x && y, s)
  sor (R (x, (y, s))) = R (x || y, s)
  snot (R (True, s)) = R (False, s)
  snot (R (False, s)) = R (True, s)
  sappend (R (x, (y, s))) = R (y ++ x, s)
  spair (R (x, (y, s))) = R ((x, y), s)
  unpair (R ((x, y), s)) = R (x, (y, s))
  sfst (R ((x, _), s)) = R (x, s)
  ssnd (R ((_, y), s)) = R (y, s)
  skip x = x
  ifThenElse (R (True, (x, (_, s)))) = R (x, s)
  ifThenElse (R (False, (_, (y, s)))) = R (y, s)

{------------------------------------------------------------------------------
-- Q1.c, for the R instance above
------------------------------------------------------------------------------}

-- Write a function that returns to the top of the stack as the result
stkEvalResult :: R (a,s) -> a
stkEvalResult (R (x,_)) = x

-- Write a function that returns to the String which is the 2nd-most top
-- of the stack as result
stkPrintEvalOutput :: R (a, (String, s)) -> String
stkPrintEvalOutput (R (_, (y, _))) = y

{------------------------------------------------------------------------------
-- Q2

Implement a compiler. The following helper function is very handy.
Except for |empty|, ALL of the rest of the code looks like
  func  = clift1 [|| \XXX -> YYY ||]
where pattern-matching on tuples for XXX is crucial.
------------------------------------------------------------------------------}

data C a = C { unC :: Q (TExp a) }

clift1 :: Q (TExp (t -> a)) -> C t -> C a
clift1 g (C x) = C [|| $$g $$x ||]

instance StackMachine C where
  empty = C [|| () ||]
  push x = clift1 [|| \s -> (x, s) ||]
  drop = clift1 [|| \(_, s) -> s ||]
  swap = clift1 [|| \(x, (y, s)) -> (y, (x, s)) ||]
  dup = clift1 [|| \(x, s) -> (x, (x, s)) ||]
  rot = clift1 [|| \(x, (y, (z, s))) -> (y, (z, (x, s))) ||]
  rot23 = clift1 [|| \(x, (y, (z, s))) -> (x, (z, (y, s))) ||]
  sadd = clift1 [|| \(x, (y, s)) -> (y + x, s) ||]
  ssub = clift1 [|| \(x, (y, s)) -> (y - x, s) ||]
  smul = clift1 [|| \(x, (y, s)) -> (y * x, s) ||]
  sleq = clift1 [|| \(x, (y, s)) -> (y <= x, s) ||]
  seql = clift1 [|| \(x, (y, s)) -> (y == x, s) ||]
  smod = clift1 [|| \(x, (y, s)) -> (y `mod` x, s) ||]
  sand = clift1 [|| \(x, (y, s)) -> (x && y, s) ||]
  sor = clift1 [|| \(x, (y, s)) -> (x || y, s) ||]
  snot = clift1 [|| \(x, s) -> (not x, s) ||]
  sappend = clift1 [|| \(x, (y, s)) -> (y ++ x, s) ||]
  spair = clift1 [|| \(x, (y, s)) -> ((x, y), s) ||]
  unpair = clift1 [|| \((x, y), s) -> (x, (y, s)) ||]
  sfst = clift1 [|| \((x, _), s) -> (x, s) ||]
  ssnd = clift1 [|| \((_, y), s) -> (y, s) ||]
  skip = clift1 [|| \s -> s ||]
  ifThenElse = clift1 [|| \(b, (x, (y, s))) -> (if b then x else y, s) ||]
-----------------------------------------------------------------

{-
  3
  Implement a partial de-compiler, meaning mapping from an
  instance of StackMachine to an Instance of classes
    IntSy, BoolSy, OrderSy, PairSy
  *only*, for the Mar18 version of the classes.

  Use RR below.  See the tutorial 10 material to get started.
-}

class IntSy (rep :: * -> *) where 
  int :: Integer -> rep Integer                   -- introduce
  add :: rep Integer -> rep Integer -> rep Integer -- computation rules
  sub :: rep Integer -> rep Integer -> rep Integer
  mul :: rep Integer -> rep Integer -> rep Integer

class BoolSy rep where
  bool :: Bool -> rep Bool
  if_ :: rep Bool -> rep a -> rep a -> rep a

  and_ :: rep Bool -> rep Bool -> rep Bool
  or_  :: rep Bool -> rep Bool -> rep Bool
  not_ :: rep Bool -> rep Bool

class OrderSy rep where
  leq :: rep Integer -> rep Integer -> rep Bool

class PairSy rep where
  pair :: rep a -> rep b -> rep (a , b) -- (,) lifted
  fst_ :: rep (a, b) -> rep a
  snd_ :: rep (a, b) -> rep b

newtype RR c a = RR { unRR :: forall s. c s -> c (a,s) }

instance StackMachine c => IntSy (RR c) where
  int x = RR (push x)
  add x y = RR (sadd . unRR y . unRR x)
  sub x y = RR (ssub . unRR y . unRR x)
  mul x y = RR (smul . unRR y . unRR x)

instance StackMachine c => BoolSy (RR c) where
  bool x = RR (push x)
  if_ x y z = RR (ifThenElse . unRR x . unRR y . unRR z)
  and_ x y = RR (sand . unRR y . unRR x)
  or_ x y = RR (sor . unRR y . unRR x)
  not_ x = RR (snot . unRR x)

instance StackMachine c => OrderSy (RR c) where
  leq x y = RR (sleq . unRR y . unRR x)

instance StackMachine c => PairSy (RR c) where
  pair x y = RR (spair . unRR x . unRR y)
  fst_ x = RR (sfst . unRR x)
  snd_ x = RR (ssnd . unRR x)

{- 4
  Write test cases for all of this:
  - 10 non-trivial programs in the StackMachine language. Each should
     have at least 8 instructions.  
  - write code that runs all these programs and check that the answer is
    correct.

  - pass all your programs through the C compiler as well. use |pprint|
     in a |main| program to print out the result code for each

  - for all the programs that can be decompiled (there should be at least 5),
      - run them through the RR interpreter, instantiated with the R interpreter
        and then the Mar18.R interpreter to "run" them
      - do the same for the PP instance of Mar22
      - (bonus) do the same for the PE instance of Apr??
-}

pprintcode :: (C () -> C a) -> IO ()
pprintcode a = do
  c <- fmap (pprint . unType) (runQ (unC (a empty)))
  putStrLn c
test1 :: (StackMachine stk) => stk () -> stk (Int, ())
test1 = push 5 >> push 10 >> sadd

-- Question 4 part 1 test cases
stk1 :: (StackMachine stk) => stk () -> stk (Int, ())
stk1 = push 5 >> push 10 >> sadd >> push 15 >> sadd >> push 20 >> sadd >> push 6 >> smod >> push 4 >> sadd

stk2 :: (StackMachine stk) => stk () -> stk (Bool, ())
stk2 = push True >> push False >> sand >> push True >> sor >> snot >> push True >> push False >> sor >> sor

stk3 :: (StackMachine stk) => stk () -> stk (String, ())
stk3 = push "H" >> push "e" >> sappend >> push "l" >> sappend >> push "l" >> sappend >> push "o" >> sappend

stk4 :: (StackMachine stk) => stk () -> stk ((Bool, Bool), ())
stk4 = push True >> push False >> spair >> sfst >> push True >> spair >> ssnd >> push True >> sor >> push False >> spair

stk5 :: (StackMachine stk) => stk () -> stk ((Int, Int), ())
stk5 = push 1 >> push 2 >> spair >> unpair >> sadd >> push 3 >> spair >> unpair >> smul >> push 11 >> spair

stk6 :: (StackMachine stk) => stk () -> stk ((String, String), ())
stk6 = push "40" >> push "02" >> spair >> unpair >> sappend >> push "01" >> spair >> unpair >> sappend >> push "059" >> spair

stk7 :: (StackMachine stk) => stk () -> stk (Int, ())
stk7 = push "Yes" >> push 1 >> swap >> push 0 >> swap >> push "Yes" >> seql >> snot >> snot >> ifThenElse

stk8 :: (StackMachine stk) => stk () -> stk (Bool, ())
stk8 = push "Yes" >> push True >> swap >> push False >> swap >> push "No" >> seql >> snot >> snot >> ifThenElse

stk9 :: (StackMachine stk) => stk () -> stk (Int, ())
stk9 = push 5 >> push 15 >> sadd >> push 25 >> smul >> push 5 >> smod >> push 100 >> sadd >> push 200 >> sadd

stk10 :: (StackMachine stk) => stk () -> stk (Bool, ())
stk10 = push True >> push True >> sand >> push True >> sand >> push True >> sand >> push True >> sand >> push False >> sand

-- Question 4 Part 3 test cases
-- This part's testing will be hold into another file called Main.hs
-- To compile it please put both of them into the same file
st1 :: Q (TExp (Int, ()))
st1 = unC $ stk1 empty

st2 :: Q (TExp (Bool, ()))
st2 = unC $ stk2 empty

st3 :: Q (TExp (String, ()))
st3 = unC $ stk3 empty

st4 :: Q (TExp ((Bool, Bool), ()))
st4 = unC $ stk4 empty

st5 :: Q (TExp ((Int, Int), ()))
st5 = unC $ stk5 empty

st6 :: Q (TExp ((String, String), ()))
st6 = unC $ stk6 empty

st7 :: Q (TExp (Int, ()))
st7 = unC $ stk7 empty

st8 :: Q (TExp (Bool, ()))
st8 = unC $ stk8 empty

st9 :: Q (TExp (Int, ()))
st9 = unC $ stk9 empty

st10 :: Q (TExp (Bool, ()))
st10 = unC $ stk10 empty

-- Question 4 part 2 testing
main :: IO ()
main = do
  pprintcode stk1
  pprintcode stk2
  pprintcode stk3
  pprintcode stk4
  pprintcode stk5
  pprintcode stk6
  pprintcode stk7
  pprintcode stk8
  pprintcode stk9
  pprintcode stk10

-- Expectations of Q4 Part 1 (C compiler) (All of those test cases works well)
{- |
(\(x_0, (y_1, s_2)) -> (y_1 GHC.Num.+ x_0, s_2)) ((\s_3 -> (4,
                                                            s_3)) ((\(x_4,
                                                                      (y_5,
                                                                       s_6)) -> (y_5 `GHC.Real.mod` x_4,
                                                                                 s_6)) ((\s_7 -> (6,
                                                                                                  s_7)) ((\(x_8,
                                                                                                            (y_9,
                                                                                                             s_10)) -> (y_9 GHC.Num.+ x_8,
                                                                                                                        s_10)) ((\s_11 -> (20,
                                                                                                                                           s_11)) ((\(x_12,
                                                                                                                                                      (y_13,
                                                                                                                                                       s_14)) -> (y_13 GHC.Num.+ x_12,
                                                                                                                                                                  s_14)) ((\s_15 -> (15,

                s_15)) ((\(x_16,

                           (y_17,

                            s_18)) -> (y_17 GHC.Num.+ x_16,

                                       s_18)) ((\s_19 -> (10,

                                                          s_19)) ((\s_20 -> (5,

                                                                             s_20)) GHC.Tuple.()))))))))))
(\(x_0, (y_1, s_2)) -> (x_0 GHC.Classes.|| y_1, s_2)) ((\(x_3,
                                                          (y_4, s_5)) -> (x_3 GHC.Classes.|| y_4,
                                                                          s_5)) ((\s_6 -> (GHC.Types.False,
                                                                                           s_6)) ((\s_7 -> (GHC.Types.True,
                                                                                                            s_7)) ((\(x_8,
                                                                                                                      s_9) -> (GHC.Classes.not x_8,
                                                                                                                               s_9)) ((\(x_10,
                                                                                                                                         (y_11,
                                                                                                                                          s_12)) -> (x_10 GHC.Classes.|| y_11,
                                                                                                                                                     s_12)) ((\s_13 -> (GHC.Types.True,

   s_13)) ((\(x_14,

              (y_15,

               s_16)) -> (x_14 GHC.Classes.&& y_15,

                          s_16)) ((\s_17 -> (GHC.Types.False,

                                             s_17)) ((\s_18 -> (GHC.Types.True,

                                                                s_18)) GHC.Tuple.())))))))))
(\(x_0, (y_1, s_2)) -> (y_1 GHC.Base.++ x_0,
                        s_2)) ((\s_3 -> (['o'], s_3)) ((\(x_4,
                                                          (y_5, s_6)) -> (y_5 GHC.Base.++ x_4,
                                                                          s_6)) ((\s_7 -> (['l'],
                                                                                           s_7)) ((\(x_8,
                                                                                                     (y_9,
                                                                                                      s_10)) -> (y_9 GHC.Base.++ x_8,
                                                                                                                 s_10)) ((\s_11 -> (['l'],
                                                                                                                                    s_11)) ((\(x_12,
                                                                                                                                               (y_13,
                                                                                                                                                s_14)) -> (y_13 GHC.Base.++ x_12,
                                                                                                                                                           s_14)) ((\s_15 -> (['e'],

         s_15)) ((\s_16 -> (['H'],

                            s_16)) GHC.Tuple.()))))))))
(\(x_0, (y_1, s_2)) -> ((x_0, y_1),
                        s_2)) ((\s_3 -> (GHC.Types.False, s_3)) ((\(x_4,
                                                                    (y_5,
                                                                     s_6)) -> (x_4 GHC.Classes.|| y_5,
                                                                               s_6)) ((\s_7 -> (GHC.Types.True,
                                                                                                s_7)) ((\((_,
                                                                                                           y_8),
                                                                                                          s_9) -> (y_8,
                                                                                                                   s_9)) ((\(x_10,
                                                                                                                             (y_11,
                                                                                                                              s_12)) -> ((x_10,
                                                                                                                                          y_11),
                                                                                                                                         s_12)) ((\s_13 -> (GHC.Types.True,
                                                                                                                                                            s_13)) ((\((x_14,

   _),

  s_15) -> (x_14,

            s_15)) ((\(x_16,

                       (y_17,

                        s_18)) -> ((x_16,

                                    y_17),

                                   s_18)) ((\s_19 -> (GHC.Types.False,

                                                      s_19)) ((\s_20 -> (GHC.Types.True,

                                                                         s_20)) GHC.Tuple.()))))))))))
(\(x_0, (y_1, s_2)) -> ((x_0, y_1), s_2)) ((\s_3 -> (11,
                                                     s_3)) ((\(x_4,
                                                               (y_5, s_6)) -> (y_5 GHC.Num.* x_4,
                                                                               s_6)) ((\((x_7, y_8),
                                                                                         s_9) -> (x_7,
                                                                                                  (y_8,
                                                                                                   s_9))) ((\(x_10,
                                                                                                              (y_11,
                                                                                                               s_12)) -> ((x_10,
                                                                                                                           y_11),
                                                                                                                          s_12)) ((\s_13 -> (3,
                                                                                                                                             s_13)) ((\(x_14,
                                                                                                                                                        (y_15,       
                                                                                                                                                         s_16)) -> (y_15 GHC.Num.+ x_14,
                                                                                                                                                                    s_16)) ((\((x_17,

           y_18),

          s_19) -> (x_17,

                    (y_18,
                                                                                                                                                                     
                     s_19))) ((\(x_20,

                                 (y_21,

                                  s_22)) -> ((x_20,

                                              y_21),

                                             s_22)) ((\s_23 -> (2,

                                                                s_23)) ((\s_24 -> (1,

                                                                                   s_24)) GHC.Tuple.()))))))))))
(\(x_0, (y_1, s_2)) -> ((x_0, y_1), s_2)) ((\s_3 -> (['0',
                                                      '5',
                                                      '9'],
                                                     s_3)) ((\(x_4,
                                                               (y_5, s_6)) -> (y_5 GHC.Base.++ x_4,
                                                                               s_6)) ((\((x_7, y_8),
                                                                                         s_9) -> (x_7,
                                                                                                  (y_8,
                                                                                                   s_9))) ((\(x_10,
                                                                                                              (y_11,
                                                                                                               s_12)) -> ((x_10,
                                                                                                                           y_11),
                                                                                                                          s_12)) ((\s_13 -> (['0',
                                                                                                                                              '1'],
                                                                                                                                             s_13)) ((\(x_14,        
                                                                                                                                                        (y_15,       
                                                                                                                                                         s_16)) -> (y_15 GHC.Base.++ x_14,
                                                                                                                                                                    s_16)) ((\((x_17,

           y_18),

          s_19) -> (x_17,

                    (y_18,

                     s_19))) ((\(x_20,

                                 (y_21,

                                  s_22)) -> ((x_20,

                                              y_21),

                                             s_22)) ((\s_23 -> (['0',

                                                                 '2'],

                                                                s_23)) ((\s_24 -> (['4',

                                                                                    '0'],

                                                                                   s_24)) GHC.Tuple.()))))))))))
(\(b_0, (x_1, (y_2, s_3))) -> (if b_0 then x_1 else y_2,
                               s_3)) ((\(x_4, s_5) -> (GHC.Classes.not x_4, s_5)) ((\(x_6,
                                                                                      s_7) -> (GHC.Classes.not x_6,
                                                                                               s_7)) ((\(x_8,
                                                                                                         (y_9,
                                                                                                          s_10)) -> (y_9 GHC.Classes.== x_8,
                                                                                                                     s_10)) ((\s_11 -> (['Y',
                                                                                                                                         'e',
                                                                                                                                         's'],
                                                                                                                                        s_11)) ((\(x_12,
                                                                                                                                                   (y_13,
                                                                                                                                                    s_14)) -> (y_13, 
                                                                                                                                                               (x_12,                                                                                                                                                                s_14))) ((\s_15 -> (0,

               s_15)) ((\(x_16,

                          (y_17,

                           s_18)) -> (y_17,

                                      (x_16,

                                       s_18))) ((\s_19 -> (1,

                                                           s_19)) ((\s_20 -> (['Y',

                                                                               'e',

                                                                               's'],

                                                                              s_20)) GHC.Tuple.())))))))))
(\(b_0, (x_1, (y_2, s_3))) -> (if b_0 then x_1 else y_2,
                               s_3)) ((\(x_4, s_5) -> (GHC.Classes.not x_4, s_5)) ((\(x_6,
                                                                                      s_7) -> (GHC.Classes.not x_6,
                                                                                               s_7)) ((\(x_8,
                                                                                                         (y_9,
                                                                                                          s_10)) -> (y_9 GHC.Classes.== x_8,
                                                                                                                     s_10)) ((\s_11 -> (['N',
                                                                                                                                         'o'],
                                                                                                                                        s_11)) ((\(x_12,
                                                                                                                                                   (y_13,
                                                                                                                                                    s_14)) -> (y_13, 
                                                                                                                                                               (x_12,                                                                                                                                                                s_14))) ((\s_15 -> (GHC.Types.False,

               s_15)) ((\(x_16,

                          (y_17,

                           s_18)) -> (y_17,

                                      (x_16,

                                       s_18))) ((\s_19 -> (GHC.Types.True,

                                                           s_19)) ((\s_20 -> (['Y',

                                                                               'e',

                                                                               's'],

                                                                              s_20)) GHC.Tuple.())))))))))
(\(x_0, (y_1, s_2)) -> (y_1 GHC.Num.+ x_0, s_2)) ((\s_3 -> (200,
                                                            s_3)) ((\(x_4,
                                                                      (y_5,
                                                                       s_6)) -> (y_5 GHC.Num.+ x_4,
                                                                                 s_6)) ((\s_7 -> (100,
                                                                                                  s_7)) ((\(x_8,
                                                                                                            (y_9,
                                                                                                             s_10)) -> (y_9 `GHC.Real.mod` x_8,
                                                                                                                        s_10)) ((\s_11 -> (5,
                                                                                                                                           s_11)) ((\(x_12,
                                                                                                                                                      (y_13,
                                                                                                                                                       s_14)) -> (y_13 GHC.Num.* x_12,
                                                                                                                                                                  s_14)) ((\s_15 -> (25,

                s_15)) ((\(x_16,

                           (y_17,

                            s_18)) -> (y_17 GHC.Num.+ x_16,

                                       s_18)) ((\s_19 -> (15,

                                                          s_19)) ((\s_20 -> (5,

                                                                             s_20)) GHC.Tuple.()))))))))))
(\(x_0, (y_1, s_2)) -> (x_0 GHC.Classes.&& y_1,
                        s_2)) ((\s_3 -> (GHC.Types.False, s_3)) ((\(x_4,
                                                                    (y_5,
                                                                     s_6)) -> (x_4 GHC.Classes.&& y_5,
                                                                               s_6)) ((\s_7 -> (GHC.Types.True,
                                                                                                s_7)) ((\(x_8,
                                                                                                          (y_9,
                                                                                                           s_10)) -> (x_8 GHC.Classes.&& y_9,
                                                                                                                      s_10)) ((\s_11 -> (GHC.Types.True,
                                                                                                                                         s_11)) ((\(x_12,
                                                                                                                                                    (y_13,
                                                                                                                                                     s_14)) -> (x_12 
GHC.Classes.&& y_13,
                                                                                                                                                                s_14)) ((\s_15 -> (GHC.Types.True,

              s_15)) ((\(x_16,

                         (y_17,

                          s_18)) -> (x_16 GHC.Classes.&& y_17,

                                     s_18)) ((\s_19 -> (GHC.Types.True,

                                                        s_19)) ((\s_20 -> (GHC.Types.True,

                                                                           s_20)) GHC.Tuple.()))))))))))
| -}
