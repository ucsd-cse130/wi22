{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveFunctor #-}

module Lec_2_16_21 where

import Text.Printf

import Debug.Trace

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
  | EAdd Expr Expr 
  | ESub Expr Expr 
  | EMul Expr Expr 
  | EVar Var 
  | ELet Var  Expr Expr
  deriving (Show)

type Var = String

type Env = [(Var, Value)]

data Value = VInt Int | VUndef
             deriving (Show) 

eval :: Env -> Expr -> Value 
eval _   (EInt n)     = VInt n 
eval env (EAdd e1 e2) = binop (+) (eval env e1) (eval env e2)
eval env (ESub e1 e2) = binop (-) (eval env e1) (eval env e2)
eval env (EMul e1 e2) = binop (*) (eval env e1) (eval env e2)
eval env (EVar v)     = lookupValue env v

binop :: (Int -> Int -> Int) -> Value -> Value -> Value
binop op (VInt v1) (VInt v2) = VInt (op v1 v2)
binop _  _         _         = VUndef 

lookupValue :: Env -> Var -> Value
lookupValue ((k,v):rest) x 
  | x == k       = v
  | otherwise    = lookupValue rest x
lookupValue [] _ = VUndef

-- >>> eval e1



