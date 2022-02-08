-- | Midterm Sp14

module MidetermSp14 where

import Prelude hiding (sum, lookup)

-- Problem 1
ans1a :: Int
ans1a =
    let x     = 10        in
    let f y z = x + y + z in
    let x     = 100       in
    let h     = f 5       in
    h x

chain :: [t -> t] -> t -> t
chain []     = \x -> x
chain (f:fs) = \x -> f (chain fs x)

ans1c :: Int
ans1c = chain [ \x -> x * x
              , \x -> 16 * x
              , \x -> x + 1
              ] 1

data Tree a
    = Leaf
    | Node a (Tree a) (Tree a)

ans0 :: Tree Int
ans0 = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)

flerb :: [a] -> Tree a
flerb []     = Leaf
flerb (x:xs) = Node x Leaf (flerb xs)

ans1f = flerb [0, 1, 2]

glub :: (t -> a) -> Tree t -> Tree a
glub f Leaf         = Leaf
glub f (Node x l r) = Node (f x) (glub f l) (glub f r)

ans1h :: Tree Int
ans1h = glub (\x -> 2 * x) ans0

-- Problem 2
sum :: Int -> Int
sum 0 = 0
sum n = n + sum (n - 1)

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

sumTR n = helper 0 n
    where
        helper :: Int -> Int -> Int
        helper acc 0 = acc
        helper acc n = helper (acc + n) (n - 1)

facTR n = helper 1 n
    where
        helper :: Int -> Int -> Int
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)

foldn :: (a -> Int -> a) -> a -> Int -> a
foldn f b n = helper b n
    where
        helper acc 0 = acc
        helper acc n = helper (f acc n) (n - 1)

sum' = foldn (\a n -> a + n) 0
fac' = foldn (\a n -> a * n) 1

-- Problem 3
data Option a = None | Some a

-- TODO What _is_ the type of this function?
safeDiv :: (Eq a, Fractional a) => a -> a -> Option a
safeDiv num 0   = None
safeDiv num den = Some (num / den)

lookup :: Eq a => a -> [(a,b)] -> Option b
lookup k []           = None
lookup k ((k',v):kvs) = if k == k' then Some v else lookup k kvs

lift1 :: (a -> b) -> Option a -> Option b
lift1 f None = None
lift1 f (Some x) = Some (f x)

lift2 :: (a -> b -> c) -> Option a -> Option b -> Option c
lift2 f None     _        = None
lift2 f _        None     = None
lift2 f (Some x) (Some y) = Some (f x y)

-- Alternatively:
lift2' :: (a -> b -> c) -> Option a -> Option b -> Option c
lift2' f None     _ = None
lift2' f (Some x) y = lift1 (f x) y

data Expr
    = Var String
    | Con Int
    | Neg Expr
    | Plus Expr Expr

eval :: [(String, Int)] -> Expr -> Option Int
eval env (Var x)      = lookup x env
eval env (Con i)      = Some i
eval env (Neg e)      = lift1 (\x -> -x) (eval env e)
eval env (Plus e1 e2) = lift2 (+) (eval env e1) (eval env e2)
