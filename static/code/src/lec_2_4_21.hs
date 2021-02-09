{-# LANGUAGE PartialTypeSignatures #-}

module Lec_2_4_21 where

import Text.Printf

import Debug.Trace

data Expr 
  = Number Double 
  | Addition Expr Expr 
  | Subtraction Expr Expr 
  | Times Expr Expr 
  deriving (Show) 

ex0 :: Expr
ex0 = Number 4.0
-- "4.0"

ex1 :: Expr
ex1 = Number 2.9
-- "2.9"

ex2 :: Expr
ex2 = Addition (Number 4.0) (Number 2.9)
-- "(4.0 + 2.9)"

ex3 :: Expr
ex3 = Times (Addition (Number 4.0) (Number 2.9)) (Number 0)
-- "((4.0 + 2.9) * 0)"

-- >>> eval ex3
-- 6.9

eval :: Expr -> Double
eval e = case e of
           (Number n)          -> {- traceRes msg -} n 
           (Addition e1 e2)    -> {- traceRes msg -} (eval e1 + eval e2)
           (Subtraction e1 e2) -> {- traceRes msg -} (eval e1 + eval e2)

           (Times e1 e2)       -> traceRes msg (eval e1 + eval e2)
         where
           msg = "Eval e = " ++ show e

traceRes :: Show a => String -> a -> a
traceRes msg x = trace (msg ++ " res = " ++ show x) x
-- foo :: String -> a -> a
-- foo = trace


fact :: Int -> Int
fact n = trace msg res
  where 
    msg = printf "When n = %d, fact n = %d" n res
    res = if n <= 0 then 1 else n * fact (n-1)


incr x = x + 1


example :: Int -> Int 
example x = trace msg out
  where
    msg = printf "When x = %s, incr x = %s" (show x) (show out)
    out = incr x 


-- >>> example 10
-- 11


data List t
  = Nil 
  | Cons t (List t)
  deriving (Show)


exList :: List Int
exList = Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil)))

exList' :: List String 
exList' = Cons "zero" (Cons "one" (Cons "two" (Cons "three" Nil)))


-- >>> [1,2,3] ++ [4,5,6]
-- [1,2,3,4,5,6]

-- >>> size exList' 
-- 4

size :: List t -> Int
size l = case l of
           Nil      -> 0
           Cons _ t -> 1 + size t

-- size (Cons 1 (Cons 2 (Cons 3 ...)))
-- 1 + size ((Cons 2 (Cons 3 ...)))
-- 1 + 1 + size (((Cons 3 ...)))
-- 1 + 1 + 1 + size (((...)))
-- 1 + 1 + 1 + .... + 1 + 0
-- 1000





append :: List t -> List t -> List t
append Nil          l2 = l2 
append (Cons h1 t1) l2 = Cons h1 (append t1 l2) 


append' l1 Nil          = l1
append' l1 (Cons h2 t2) = append' (Cons h2 l1) t2

rev :: List a -> List a
rev l = append' Nil l

-- append' Nil (Cons h1 (Cons h2 (Cons h3 Nil)))

-- => append' (Cons h1 Nil) ((Cons h2 (Cons h3 Nil)))

-- => append' (Cons h2 (Cons h1 Nil)) (((Cons h3 Nil)))

-- => append' (Cons h3 (Cons h2 (Cons h1 Nil))) (((Nil)))

-- =>(Cons h3 (Cons h2 (Cons h1 Nil)))


append'' :: List t -> List t -> List t 
append'' Nil     l2      = Nil
append'' l1      Nil     = Nil
append'' (Cons h1 t1) (Cons h2 t2) = Cons h1 (Cons h2 (append'' t1 t2))

-- >>> append'' (Cons 1 (Cons 2 (Cons 3 Nil))) (Cons 4 (Cons 5 (Cons 6 Nil)))
-- >>> Cons 1 (Cons 4 (append'' ((Cons 2 (Cons 3 Nil))) ((Cons 5 (Cons 6 Nil)))
-- >>> Cons 1 (Cons 4 (Cons 2 (Cons 5 (append'' (((Cons 3 Nil))) (((Cons 6 Nil)))
-- >>> Cons 1 (Cons 4 (Cons 2 (Cons 5 (Cons 3 (Cons 6 (append'' (((Nil))) (((Nil)))
-- >>> Cons 1 (Cons 4 (Cons 2 (Cons 5 (Cons 3 (Cons 6 Nil))))) 

data Tree 
  = Node Int Tree Tree
  | Leaf
  deriving (Show)

exTree :: Tree
exTree = Node 1  
          (Node 2 
            (Node 3 
              Leaf 
              Leaf) 
            Leaf)
          (Node 4 
            Leaf 
            Leaf)

-- >>> treeMax exTree
-- 4

treeMax :: Tree -> Int
treeMax t = case t of
              Leaf -> 0
              Node v l r -> maximum [v, treeMax l, treeMax r]

-- >>> maximum ["cat", "horse", "mouse", "zebra", "apple"]
-- "zebra"

-- >>> total exTree
-- 10

total :: Tree -> Int
total t = case t of
            Leaf       -> 0
            Node v l r -> v + total l + total r

-- >>> depth exTree
-- 3

depth :: Tree -> Int
depth t = case t of
            Leaf       -> 0
            Node v l r -> 1 + mymax (depth l) (depth r) 

mymax :: Ord p => p -> p -> p
mymax x y = if x > y then x else y

-- >>> mymax "cat" "zerb"
-- "zerb"



exList2 :: List Int
exList2 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))


-- >>> listToTree exList2
-- Node 1 Leaf (Node 2 Leaf (Node 3 Leaf (Node 4 Leaf Leaf)))

listToTree :: List Int -> Tree
listToTree  Nil       = Leaf 
listToTree (Cons h t) =  Node h Leaf (listToTree t)



-- fac :: Int -> Int
-- fac n 
--   | n <= 0    = 1
--   | otherwise = n * fac (n-1)

{- 
def fac(n):
  res = 1
  i   = 1
  while (i <= n):
    res = res * i
    i   = i + 1 
  return res

def fib(n):
  a,b = 0,1
  i   = 1
  while (i <= n):
    a, b = b, a+b 
    i = i + 1
  return b

-}

fibb :: Int -> Int
fibb n = if n <= 1 then 1 else fibb (n-1) + fibb (n-2)

fib :: Int -> Int 
fib n = loop 1 0 1 
  where
    loop i a b
      | i <= n    = loop (i+1) b (a+b)
      | otherwise = b

fac :: Int -> Int
fac n = loop 1 1 
  where
   loop i res 
     | i <= n    = loop (i + 1) (res * i)
     | otherwise = res

{- fac 4 ==> 
   loop 1 1 => 
   loop (1+1) (1*1) => 
   loop (1+1+1) (1*1*2) => 
   loop 4 (1*2*3) => 
   loop 5 (1*2*3*4) => 24

 -}
