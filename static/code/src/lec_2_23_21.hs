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




{- 
let incr = \x -> x + 1
in
  incr 50
-}

exIncr :: Expr
exIncr = 
  ELet "incr" (ELam "x" (EAdd (EVar "x") (EInt 1))) 
    (EApp (EVar "incr") (EInt 50))

data Expr 
  = EInt Int 
  | EVar Var 
  | EAdd Expr Expr 
  | ESub Expr Expr 
  | EMul Expr Expr 
  | ELet Var  Expr Expr
  | ELam Var  Expr       -- \x -> e    function definition
  | EApp Expr Expr       -- e1 e2      function call
  deriving (Show)

type Var = String

type Env = [(Var, Value)]

traceS :: Show a => String -> a -> a
traceS msg x = trace ("TRACE: " ++ msg ++ " result = " ++ show x) x

eval :: Env -> Expr -> Value 
eval env e = traceS msg (evalHelper env e)
  where
    msg    = "Eval e = " ++ show e ++ " env = " ++ show env 

evalHelper :: Env -> Expr -> Value
evalHelper _   (EInt n)       = VInt n 
evalHelper env (EAdd e1 e2)   = binop (+) (eval env e1) (eval env e2)
evalHelper env (ESub e1 e2)   = binop (-) (eval env e1) (eval env e2)
evalHelper env (EMul e1 e2)   = binop (*) (eval env e1) (eval env e2)
evalHelper env (EVar v)       = lookupValue env v
evalHelper env (ELet x e1 e2) = eval extEnv e2
  where 
    v1                        = eval env e1
    extEnv                    = (x, v1) : env 
-- evalHelper env (ELam x e)     = _fixme
-- evalHelper env (EApp e1 e2)   = _fixme




data Value = VInt Int 
           | VFun Var Expr    -- param, body
           | VUndef
           deriving (Show) 



-- >>> eval [] q1

{- 
TRACE: Eval e = EInt 10 env = [] result = VInt 10
TRACE: Eval e = EInt 100 env = [("x",VInt 10)] result = VInt 100
TRACE: Eval e = EVar "x" env = [("x",VInt 100),("x",VInt 10)] result = VInt 100
TRACE: Eval e = EInt 1 env = [("x",VInt 100),("x",VInt 10)] result = VInt 1
TRACE: Eval e = EAdd (EVar "x") (EInt 1) env = [("x",VInt 100),("x",VInt 10)] result = VInt 101
TRACE: Eval e = ELet "x" (EInt 100) (EAdd (EVar "x") (EInt 1)) env = [("x",VInt 10)] result = VInt 101
TRACE: Eval e = EVar "x" env = [("x",VInt 10)] result = VInt 10
TRACE: Eval e = EAdd (ELet "x" (EInt 100) (EAdd (EVar "x") (EInt 1))) (EVar "x") env = [("x",VInt 10)] result = VInt 111
TRACE: Eval e = ELet "x" (EInt 10) (EAdd (ELet "x" (EInt 100) (EAdd (EVar "x") (EInt 1))) (EVar "x")) env = [] result = VInt 111

-}


q1 :: Expr
q1 = 
  ELet "x" (EInt 10)
    (EAdd
      ( ELet "x" (EInt 100)
        (EAdd (EVar "x") (EInt 1))
      )
      (
        EVar "x"
      )
    )


-- >>> eval [] q2
-- VInt 101

q2 :: Expr
q2 = 
  ELet "x" (EInt 10)
    ( ELet "y" (EInt 100)
        (EAdd (EVar "x") (EVar "y"))
    )


-- >>> eval [] q3
-- VInt 101

q3 :: Expr
q3 = 
  ELet "x" (EInt 0)
    ( ELet "x" (EInt 100)
        (EAdd (EVar "x") (EInt 1))
    )

rfoo :: Int -> Int -> Int 
rfoo _ 0 = error "boo shame on you" 
rfoo n m = n `div` m

-- >>> eval [] (EAdd (EStr "super") (EStr "Mario"))
-- VStr "superMario"

-- VStr "superMario"








binop :: (Int -> Int -> Int) -> Value -> Value -> Value
binop op (VInt v1) (VInt v2) = VInt (op v1 v2)
binop _  _         _         = VUndef 

lookupValue :: Env -> Var -> Value
lookupValue ((k,v):rest) x 
  | x == k       = v
  | otherwise    = lookupValue rest x
lookupValue [] _ = VUndef

-- >>> eval e1


--------------------------------------------------------------------
-- >>> emily 
-- 201

emily :: Integer
emily = 
  let x = 0 
  in 
    let x = 100
    in
      x + 1
  + x
  
{- 

let x = 0 
in 
  (
    let x = 100
    in
      x + 1
  )
  +
  x

let x = 0 
in 
    let x = 100
    in
      x + 1
  +
  x

-}

