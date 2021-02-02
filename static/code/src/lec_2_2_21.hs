{-# LANGUAGE PartialTypeSignatures #-}

module Lec_2_2_21 where

import Text.Printf


data Nat
  = Zero
  | Next Nat
  deriving (Show)

-- >>> nums
-- [Zero,Three]

one :: Nat
one = Next Zero 

two :: Nat
two = Next one

three :: Nat
three = Next two 

nums :: [Nat]
nums = [Zero, one, two, three]

-- >>> nums
-- [Zero,  Next Zero,  Next (Next Zero), Next (Next (Next Zero))]

-- >>> toInt Zero  
-- 0

-- >>> toInt one
-- 1

-- >>> toInt two
-- 2

toInt :: Nat -> Int
toInt       Zero   = 0
toInt (Next m)     = 1 + toInt m


fromInt :: Int -> Nat
fromInt i 
  | i <= 0    = Zero 
  | otherwise = Next (fromInt (i - 1))

add :: Nat -> Nat -> Nat
add Zero     m = m 
add (Next n) m = Next (add n m)

{-
  
add (Next (Next (Next Zero))) m

==> Next (add (Next (Next Zero)) m)

==> Next (Next (add (Next Zero) m))

==> Next (Next (Next (add Zero m)))

==> Next (Next (Next m))




-}



sub :: Nat -> Nat -> Nat
sub Zero     _        = Zero
sub n        Zero     = n
sub (Next n) (Next m) = sub n m

-- sub (Next (Next (Next Zero))) (Next (Next Zero))
-- sub ((Next (Next Zero))) ((Zero))
{- 
sub three two

sub (Next (Next (Next Zero))) (Next (Next Zero))
          ^--- n                    ^---- m

==> sub (Next (Next Zero)) (Next Zero)
              ^--- n             ^---- m

==> sub (Next Zero) (Zero)

==> (Next Zero) 

-}

-- >>> sub three zero
-- three
-- >>> sub three one
-- two


-- add n m = case n of 
--             Zero    -> m 
--             Next n' -> Next (add n' m) 

-- >>> add two three
-- Next (Next (Next (Next (Next Zero))))






{- 
-}

{- 
foo 2 

==> Next (foo (2-1))
==> Next (foo 1)
==> Next (Next (foo 0))
==> Next (Next Zero) 


-}

{-

(4.0 + 2.9) + 3.2

-} 


data Expr 
  = Num Double 
  | Add Expr Expr 
  | Sub Expr Expr 
  | Mul Expr Expr 
  deriving (Show) 

ex0 :: Expr
ex0 = Num 4.0
-- "4.0"

ex1 :: Expr
ex1 = Num 2.9
-- "2.9"

ex2 :: Expr
ex2 = Add (Num 4.0) (Num 2.9)
-- "(4.0 + 2.9)"

ex3 :: Expr
ex3 = Add (Add (Num 4.0) (Num 2.9)) (Num 3.2)
-- "((4.0 + 2.9) + 3.2)"

-- >>> eval e3
-- 10.100000000000001


eval :: Expr -> Double
eval (Num n)     = n 
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

exprToString :: Expr -> String