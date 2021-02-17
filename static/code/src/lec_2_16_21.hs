{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveFunctor #-}

module Lec_2_16_21 where

import Text.Printf

import Debug.Trace

inc :: Int -> Int
inc x = x + 1

dec :: Int -> Int
dec x = x - 1 

dub :: Int -> Int
dub x = x * 2

foo :: Int -> Int
-- foo x = dec (dub (inc x))
foo = dec . dub . inc 

--  foo = dub . inc

-- foo x = dub (inc x)

-- f . g = \x -> f (g x)



expr0 :: Expr
expr0 = EAdd (EVar "x") (EInt 1)

{- 

foo + bar

  foo.__add__(bar) 

  "cat".__add__(other) ==> "cat" ++ str(other)

  12.__add__(other)    ==> if isinstance(other) == int:
                             12 + other
                           else:
                             str(12) ++ str(other)

-}


{- 
  e := n 
     | e1 + e2 
     | e1 - e2 
     | e1 * e2

     | x, y, z, ... 

(4 + 12) - 5


(10 + a) * b

 -}


ex0 :: Expr 
ex0 = ESub (EAdd (EInt 4) (EInt 12)) (EInt 5)

ex1 :: Expr
ex1 = EAdd (EVar "x") (EInt 1)

ex2 :: Expr
ex2 = EMul (EAdd (EInt 10) (EVar "a")) (EVar "b")

ex3 :: Expr
ex3 = ELet "x" (EInt 0)
        ( 
          ELet "x" (EInt 100) 
            (
              EAdd (EVar "x") (EInt 1)
            )
        )

quiz :: Int
quiz = 
  let x = 0 in
   let y = 100 in
    let z = x - y in
      x + y + z

-- >>> eval [("a", VInt 100), ("b", VInt 5)] ex2
-- VInt 550






data Expr 
  = EInt Int 
  | EVar Var 
  | EStr String
  | EAdd Expr Expr 
  | ESub Expr Expr 
  | EMul Expr Expr 
  | ELet Var  Expr Expr
  deriving (Show)

type Var = String

type Env = [(Var, Value)]

data Value = VStr String | VInt Int | VUndef
             deriving (Show) 



eval :: Env -> Expr -> Value 
eval _   (EInt n)     = VInt n 
eval _   (EStr s)     = VStr s 
eval env (EAdd e1 e2) = binop (+) (eval env e1) (eval env e2)
eval env (ESub e1 e2) = binop (-) (eval env e1) (eval env e2)
eval env (EMul e1 e2) = binop (*) (eval env e1) (eval env e2)
eval env (EVar v)     = lookupValue env v

-- >>> eval [] (EAdd (EStr "super") (EStr "Mario"))
-- VStr "superMario"

-- VStr "superMario"








binop :: (Int -> Int -> Int) -> Value -> Value -> Value
binop op (VInt v1) (VInt v2) = VInt (op v1 v2)
binop _  (VStr s1) (VStr s2) = VStr (s1 ++ s2)
binop _  _         _         = VUndef 

lookupValue :: Env -> Var -> Value
lookupValue ((k,v):rest) x 
  | x == k       = v
  | otherwise    = lookupValue rest x
lookupValue [] _ = VUndef

-- >>> eval e1


--------------------------------------------------------------------

{- 
   1 1

[.... 1,2,3,4]
[.... 5,6,7,8]

      6,9,1,2

  (0, []) == (4,8) => (1, [2]) --> (1, [1, 2]) == (2, 6) ==> (0, [9,1,2]) == (1,5) ==> (0, [6,9,1,2])

  4,  3,  2,  1
  8,  7,  6,  5

  


-}

