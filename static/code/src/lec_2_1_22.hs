module Lec_2_3_22 where
import Test.QuickCheck (InfiniteList)

data Nat 
  = Zero 
  | OnePlus Nat

data List a 
  = Nil 
  | Cons a (List a)
  deriving (Show)

len :: List a -> Int
len Nil = 0
len (Cons x xs) = 1 + len xs

lenTR :: List a -> Int
lenTR l = loop 0 l
  where
    loop res Nil    = res
    loop res (Cons _ t) = loop (res+1) t 

{- 

def len(l):
  res = 0
  while true:
    if l == Nil: 
      return res
    else:
      (res, l) = (res + 1, tail(l))
  return res

-}


-- >>> [1,2,3] ++ [5,6,7]
-- [1,2,3,4,5,6,7]

l_123 :: List Int 
l_123 = Cons 1 (Cons 2 (Cons 3 Nil))

l_456 :: List Int 
l_456 = Cons 4 (Cons 5 (Cons 6 Nil))

l_123456 :: List Int
l_123456 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil))) ))

append :: List a -> List a -> List a
append Nil         ys  = ys
append (Cons x xs) ys  = Cons x (append xs ys)

rev :: List a -> List a
rev l = case l of
  Nil -> Nil 
  Cons h t -> append (rev t) (Cons h Nil)

-- >>> rev l_123
-- Cons 3 (Cons 2 (Cons 1 Nil))

-- append l_123 l_456
-- (A) Nil
-- (B) l_123
-- (C) l_456    <<<
-- (D) l_123456


-- append (Cons 2 (Cons 3 Nil)) l_456 
-- ==> Cons 2 (append (Cons 3 Nil) l_456)
-- ==> Cons 2 (Cons 3 (append Nil l456))
-- ==> Cons 2 (Cons 3 l456)
-- >>> append (Cons 3 Nil) l_456 
-- Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))



-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil))) ))


-- >>> Cons "cat" (Cons "dog" Nil)
-- Cons "cat" (Cons "dog" Nil)



-- (4.0 + 2.9)  * (3.78 - 5.92)

e0 :: Expr
e0 = EBin Mul 
        (EBin Add 
            (ENum 4.0) 
            (ENum 2.9)
        )
        (EBin Sub 
            (ENum 3.78) 
            (ENum 5.92)
        )

calc :: Expr -> Double
calc e = case e of
  ENum x -> x
  EBin Add e1 e2 -> calc e1 + calc e2 
  EBin Sub e1 e2 -> calc e1 - calc e2 
  EBin Mul e1 e2 -> calc e1 * calc e2 

-- >>> calc e0 
-- -14.766000000000002

-- type Op = Double -> Double -> Double

data Op 
  = Add 
  | Sub 
  | Mul 
  deriving (Show)

data Expr 
    = ENum Double
    | EBin Op Expr Expr
    deriving (Show)

    
data Tree a = Leaf | Node a (Tree a) (Tree a) 
  deriving (Show)

myTree :: Tree Int
myTree = 
  Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) 
         (Node 4 Leaf Leaf)

-- >>> height myTree
-- 3


height :: Tree a -> Int
height t = case t of  
  Leaf -> 0
  Node _ l r -> 1 + larger (height l) (height r)

larger :: Int -> Int -> Int
larger x y = if x >= y then x else y

----

{- 

<sumTo 3>
==> 3 + < sumTo 2 >
==> 3 + < 2 + <sumTo 1> >
==> 3 + < 2 + < 1  + < sumTo 0 > > >
==> 3 + < 2 + < 1  + < 0 > > >

def sumTo(n):

  (res, 0) = (0, 0)

  while true: 
    if (i <= n):
      (res, i) = (res + i, i + 1)
    else 
      break
  
  return res



sumTo n = loop (0, 0)
  where
    loop (res, i) = 
      if (i <= n):
        -- (res, i) = (res + i, i + 1)
        loop (res + i, i + 1)
      else 
        res







-}

sumTo' :: Int -> Int
sumTo' 0 = 0
sumTo' n = n + sumTo' (n - 1)

sumTo :: Int -> Int
sumTo n = loop (0, 0)
  where
    loop (res, i) 
      | i <= n    = loop (res + i, i + 1)
      | otherwise = res

-- >>> sumTo 5
-- ==> loop (0, 0)
-- ==> loop (0, 1)
-- ==> loop (1, 2)
-- ==> loop (3, 3)
-- ==> loop (6, 4)
-- ==> loop (10, 5)
-- ==> loop (15, 6)
-- ==> 15

fac :: Int -> Int
fac n = if n <= 0 then 1 else n * fac (n-1)

facTR :: Int -> Int
facTR n = loop (1, 1)
  where
    loop (res, i) 
      | i <= n = loop (res * i, i + 1)
      | otherwise = res

-- >>> facTR 5
-- 120
