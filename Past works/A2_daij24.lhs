\begin{code}
module A2_daij24 where -- Mentioned in the lecture (necessary for function calls)
iter :: Integer -> (a -> a) -> (a -> a)
iter n f
  | n > 0 = f . iter (n - 1) f          -- iter.1
  | otherwise = id                      -- iter.2
\end{code}



\begin{itemize}
\item Found Theorem 1: id a = a
\item Found Theorem 2: f . g = \ x -> f (g x)
\item Those Theorems are from lectures, tutorials and other Haskell tutorial websites
\item They are showing different letters and operators' functions 
\item
\item
\item Our goal: iter n id = id
\item 
\item Base case: n = 0
\item LHS:
\item      iter 0 id
\item    ={ iter.2 }
\item      id = RHS
\item Therefore, LHS = RHS, and the base case holds!
\item
\item
\item Induction step: n = n + 1
\item LHS:
\item      iter (n + 1) id
\item    ={ iter.1 }
\item      id . iter (n + 1 - 1) id
\item    ={ evaluation }
\item      id . iter n id
\item    ={ Found Theorem 2 }
\item      id (iter n id)
\item    ={ Found Theorem 1 }
\item      iter n id
\item    ={ Induction Hypothesis }
\item      id = RHS
\item Therefore, LHS = RHS, and the induction step holds!
\end{itemize}



\begin{itemize}
\item Found Theorem 1: id a = a
\item Found Theorem 2: f . g = \ x -> f (g x)
\item Those Theorems are from lectures, tutorials and other Haskell tutorial websites
\item They are showing different letters and operators' functions 
\item
\item
\item As mentioned in first lecture we have those 
\item map :: (a -> b) -> [a] -> [b]
\item map f []       = []                   -- map.1
\item map f (x : xs) = f x : map f xs       -- map.2
\item
\item
\item Our goal: map f ( ys ++ zs ) = map f ys ++ map f zs
\item 
\item Base case: ys = []
\item LHS:
\item      map f ( ys ++ zs )
\item    ={ identity of ++ }
\item      map f zs
\item
\item
\item RHS:
\item      map f [] ++ map f zs
\item    ={ map.1 }
\item      [] ++ map f zs
\item    ={ identity of ++ }
\item      map f zs
\item Therefore, LHS = RHS, and the base case holds!
\item
\item
\item Induction step: ys = (y:ys)
\item LHS:
\item      map f ( (y:ys) ++ zs )
\item    ={ ++.2 }
\item      map f ( y:(ys ++ zs) )
\item    ={ map.2 }
\item      f y : map f (ys ++ zs)
\item    ={ Induction Hypothesis }
\item      f y : map f ys ++ map f zs
\item    ={ map.2 }
\item      map f (y:ys) ++ map f zs = RHS
\item Therefore, LHS = RHS, and the induction step holds!
\end{itemize}



\begin{itemize}
\item concat :: [[a]] −> [a]
\item concat = foldr (++) [] −− concat.1
\item
\item As mentioned in first lecture we have those 
\item map :: (a -> b) -> [a] -> [b]
\item map f []       = []                   -- map.1
\item map f (x : xs) = f x : map f xs       -- map.2
\item
\item foldr k z = go
\item     where
\item         go []     = z                 -- foldr.1
\item         go (y:ys) = y `k` go ys       -- foldr.2
\item
\item Our goal: concat (map (map f) xs) = map f (concat xs)
\item
\item Base case: xs = []
\item LHS:
\item      concat (map (map f) [])
\item    ={ map.1 }
\item      concat []
\item    ={ concat.1 }
\item      (foldr (++) []) []
\item    ={ foldr.1 }
\item      []
\item
\item
\item RHS:
\item      map f (concat [])
\item    ={ concat.1 }
\item      map f ((foldr (++) []) [])
\item    ={ foldr.1 }
\item      map f []
\item    ={ map.1 }
\item      []
\item Therefore, LHS = RHS, and the base case holds!
\item
\item
\item Induction step: xs = (x:xs)
\item RHS:
\item      map f (concat (x:xs))
\item    ={ concat.1 }
\item      map f (foldr (++) [] (x:xs))
\item    ={ foldr.2 }
\item      map f (x ++ foldr (++) [] (xs))
\item    ={ evaluation, map.2 }
\item      f x ++ map f (foldr (++) [] (xs))
\item    ={ concat.1 }
\item      f x ++ map f (concat (xs))
\item    ={ Induction Hypothesis }
\item      f x ++ concat (map (map f) xs)
\item    ={ concat.1 }
\item      f x ++ foldr (++) [] (map (map f) xs)
\item    ={ foldr.2, concat.1 }
\item      concat (map (map f) (x:xs)) = LHS
\item Therefore, LHS = RHS, and the induction step holds!
\end{itemize}



\begin{itemize}
\item filter :: (a -> Bool) -> [a] -> [a]
\item filter _pred []    = []                  -- filter.1
\item filter pred (x:xs)
\item  | pred x         = x : filter pred xs   -- filter.2
\item  | otherwise      = filter pred xs       -- filter.3
\item
\item p &&& q = \x −> p x && q x               -- &&&.1
\item
\item Our goal: filter p ( filter q xs ) = filter ( p &&& q ) xs
\item 
\item Base case: xs = []
\item LHS:
\item      filter p ( filter q [] )
\item    ={ filter.1 }
\item      filter p []
\item    ={ filter.1 }
\item      []
\item
\item RHS:
\item      filter ( p &&& q ) []
\item    ={ filter.1 }
\item      [] = RHS
\item Therefore, LHS = RHS, and the base case holds!
\item
\item
\item Induction step: xs = (x:xs), assume p x = q x = True (assumption 1)
\item LHS:
\item      filter p ( filter q (x:xs) )
\item    ={ filter.2 }
\item      filter p ( x : filter q xs )
\item    ={ filter.2 }
\item      x : filter p ( filter q xs )
\item    ={ Induction Hypothesis }
\item      x : filter ( p &&& q ) xs
\item
\item RHS:
\item      filter ( p &&& q ) (x:xs)
\item    ={ &&&.1 }
\item      filter ( \x −> p x && q x ) (x:xs)
\item    ={ filter.2, assumption }
\item      x : filter ( \x −> p x && q x ) (xs)
\item    ={ &&&.1 }
\item      x : filter ( p &&& q ) xs
\item Therefore, LHS = RHS, and the induction step holds!
\item (Other assumptions about values of p x and q x also works well on induction proving, however it is too long and they are not loaded here)
\end{itemize}



\begin{code}
data Expr =                             -- Given data types
    Lit Integer                         -- Expression can be individual integers
  | Expr :+: Expr                       -- or add 2 parts together 
  | Expr :−: Expr                       -- or minus 2 parts together

size :: Expr -> Integer                 -- given function types
size (Lit a) = 0                        -- Individual integers should have size 0
size (a :+: b) = size a + 1 + size b    -- each plus should take up 1
size (a :−: b) = size a + 1 + size b    -- each minus should take up 1
\end{code}



\begin{code}
data Expr' =                            -- given data types
    Lit' Integer                        -- Expression can be individual integers
  | Expr' :++: Expr'                    -- or add 2 parts together
  | Expr' :--: Expr'                    -- or minus 2 parts together
  | Expr' :**: Expr'                    -- or multiply 2 parts together
  | Expr' ://: Expr'                    -- or divide 2 parts together

show' :: Expr' -> String -- given function types
show' (Lit' a) =  "Lit' " ++ show a                                    -- we make integers being printed together
show' (a :++: b) = "(" ++ show' a ++ " :++: " ++ show' b ++ ")"        -- add expressions should be expressed in string
show' (a :--: b) = "(" ++ show' a ++ " :--: " ++ show' b ++ ")"        -- subtract expressions should be expressed in string
show' (a :**: b) = "(" ++ show' a ++ " :**: " ++ show' b ++ ")"        -- multiply expressions should be expressed in string
show' (a ://: b) = "(" ++ show' a ++ " ://: " ++ show' b ++ ")"        -- divide expressions should be expressed in string

size' :: Expr' -> Integer -- given function types
size' (Lit' a) = 0                                                  -- Individual integers should have size 0
size' (a :++: b) = size' a + 1 + size' b                            -- each plus should take up 1
size' (a :--: b) = size' a + 1 + size' b                            -- each minus should take up 1
size' (a :**: b) = size' a + 1 + size' b                            -- each multiply should take up 1
size' (a ://: b) = size' a + 1 + size' b                            -- each divide should take up 1

eval' :: Expr' -> Integer -- given function types
eval' (Lit' a) = a                                                  -- Extract integer value
eval' (a :++: b) = (eval' a) + (eval' b)                            -- evaluate add
eval' (a :--: b) = (eval' a) - (eval' b)                            -- evaluate subtract
eval' (a :**: b) = (eval' a) * (eval' b)                            -- evaluate multiply
eval' (a ://: b) = do                                               -- evaluate division
  if eval' b == 0                                                   -- if the division is "divided by 0"
    then error "Error: Division by zero"                            -- then raise error
  else div (eval' a) (eval' b)                                      -- or we will evaluate its value
\end{code}



\begin{code}
-- There is nothing we can do with function "show", because function "show" only needs to transfer the whole thing into a string
-- It does not need to check whether this expression makes sense or not

-- There is nothing we can do with function "size", since function "size" only needs to find the expression's size
-- It does not need to check whether this expression makes sense or not

-- When we have a division by 0, it will raise an error "Error: Division by zero"
\end{code}



\begin{code}
data Expr2 =                                                        -- given function types
    Lit2 Integer                                                    -- Individual Integers
  | Op Ops Expr2 Expr2                                              -- Expressions for 4 kinds of expressions

data Ops = Add | Sub | Mul | Div                                    -- 4 types of operators

show2 :: Expr2 -> String -- given function types
show2 (Lit2 a) = "(Lit2 " ++ show a ++ ")"                          -- Express individual integers with data types
show2 (Op Add a b) = "(Op Add " ++ show2 a ++ show2 b ++ ")"        -- Express add operation
show2 (Op Sub a b) = "(Op Sub " ++ show2 a ++ show2 b ++ ")"        -- Express subtract operation
show2 (Op Mul a b) = "(Op Mul " ++ show2 a ++ show2 b ++ ")"        -- Express multiplication operation
show2 (Op Div a b) = "(Op Div " ++ show2 a ++ show2 b ++ ")"        -- Express division operation

size2 :: Expr2 -> Integer -- given function types
size2 (Lit2 a) = 0                                                  -- Individual integers are not operation so 0
size2 (Op Add a b) = size2 a + 1 + size2 b                          -- Each add operation is 1-length
size2 (Op Sub a b) = size2 a + 1 + size2 b                          -- Each sub operation is 1-length
size2 (Op Mul a b) = size2 a + 1 + size2 b                          -- Each mul operation is 1-length
size2 (Op Div a b) = size2 a + 1 + size2 b                          -- Each div operation is 1-length

eval2 :: Expr2 -> Integer -- given function types
eval2 (Lit2 a) = a                                                  -- Extract integer object value
eval2 (Op Add a b) = (eval2 a) + (eval2 b)                          -- evaluate add expressions
eval2 (Op Sub a b) = (eval2 a) - eval2 b                            -- evaluate sub expressions
eval2 (Op Mul a b) = eval2 a * eval2 b                              -- evaluate mul expressions
eval2 (Op Div a b) = do                                             -- evaluate div expressions
  if eval2 b == 0                                                   -- if it is a "divide by 0" division
    then error "Error: Division by zero"                            -- then raise error
  else div (eval2 a) (eval2 b)                                      -- or go ahead for division
\end{code}

\begin{code}
-- Change 1: data Ops = Add | Sub | Mul | Div | Rem
-- Change 2: show2 (Op Rem a b) = "(" ++ show2 a ++ " Rem " ++ show2 b ++ ")"
-- Change 3: size2 (Op Rem a b) = size2 a + 1 + size2 b
-- Change 4: eval2 (Op Div a b) = do 
--  if eval2 b == 0
--    then mod (eval2 a) (eval2 b)
--  else error "Error: Division by zero"
\end{code}



\begin{code}
join :: (a -> c) -> (b -> d) -> Either a b -> Either c d            -- given function types
join f g (Left a) = Left (f a)                                      -- If Left a then replace a with c
join f g (Right b) = Right (g b)                                    -- If Right b then replace b with d
\end{code}



\begin{code}
-- Assume each GTree node can only have 2 branches at most (so Branches only have 2 elements at most)
data GTree a = Leaf a | GNode [GTree a]                             -- given data types
countG :: GTree a -> Integer                                        -- self function types
countG (Leaf a) = 1                                                 -- count each leaf
countG (GNode []) = 0                                               -- Empty node do not have leaves
countG (GNode b) = sum (map countG b)                               -- recursion to scan through lower levels

depthG :: GTree a -> Integer                                        -- self function types
depthG (Leaf a) = 0                                                 -- Individual leaves do not have depth
depthG (GNode []) = 0                                               -- Empty node does not have depth
depthG (GNode b) = 1 + max (depthG (head b)) (depthG (last b))      -- Checks for 2 parts on whether they have the largest depth

sumG :: GTree Int -> Int                                            -- self function types
sumG (Leaf x) = x                                                   -- extract integer object value
sumG (GNode []) = 0                                                 -- Empty node does not have leaves with values
sumG (GNode b) = sum (map sumG b)                                   -- get sum of each part and sum them up

inG :: (Eq a) => GTree a -> a -> Bool                               -- self function types
inG (Leaf x) y = x == y                                             -- to check whether this it is in an integer
inG (GNode x) y = inG2 x y                                          -- to scan through all parts and check each part

inG2 :: (Eq a) => [GTree a] -> a -> Bool                            -- self function types
inG2 [] y = False                                                   -- helper function: if empty list then return false
inG2 x y = (inG (head x) y) || (inG2 (tail x) y)                    -- helper function: if not empty then matches each part

mapG :: (Eq a) => (a -> a) -> GTree a -> GTree a                    -- self function types
mapG f (Leaf x) = Leaf (f x)                                        -- exchange leaf value with output of function
mapG f (GNode x) = GNode (mapG2 f x)                                -- Solve list problems with helper function
 
mapG2 :: (Eq a) => (a -> a) -> [GTree a] -> [GTree a]               -- self function types
mapG2 f [] = []                                                     -- helper function map on empty list should return empty list
mapG2 f x = [mapG f (head x)] ++ (mapG2 f (tail x))                 -- use function on each part and connect them together

flattenG :: (Eq a) => GTree a -> [GTree a]                          -- self function types
flattenG (Leaf x) = [Leaf x]                                        -- individual leaf are extracted 
flattenG (GNode x) = flattenG2 x                                    -- go through all parts and extract leaves on each part

flattenG2 :: (Eq a) => [GTree a] -> [GTree a]                       -- self function types
flattenG2 [] = []                                                   -- nothing we can do with empty list
flattenG2 x = flattenG (head x) ++ (flattenG2 (tail x))             -- go through each parts' element and flatten each part
\end{code}



\begin{code}
separate :: [a] -> [[[a]]]
separate [] = [[]]
separate (x:xs) = let recur = separate xs
                      split = do
                        partition <- recur
                        return $ [x] : partition
                      noSplit = do
                        (y:ys) <- recur
                        return $ (x:y):ys
                  in split ++ noSplit

\end{code}