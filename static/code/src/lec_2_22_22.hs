{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lec_2_17_22 where

import qualified Data.Map as M

import Prelude hiding (lookup)

inc :: Int -> Int
inc x = x + 1


{-
e ::= n
    | e1 + e2
    | e1 - e2
    | e1 * e2
    | ( e )
    | x, y, z, ...

    | LET x = e1 IN e2
-}

type Id = String
data Expr
    = ENum Int              -- ^ n
    | EBin BOp Expr Expr    -- ^ e1 `op` e2
    | EVar Id               -- ^ x, y, z, ...
    | ELet Id  Expr Expr    -- ^ let x = e1 in e2

    | ELam Id  Expr         -- ^ \x -> e
    | EApp Expr Expr        -- ^ (e1 e2)
    deriving (Show)

data Value
  = VInt     Int
  | VClosure Id Expr Env    -- ^ <param, body, frozen-env>
  deriving (Show)

data BOp = Add | Sub | Mul deriving (Show)

type Env = [Bind]


{-
let incr = \x -> x + 1
in
  incr 10
-}

ex_incr :: Expr
ex_incr = ELet "incr" (ELam "x" (EBin Add (EVar "x") (ENum 1)))
            (EApp (EVar "incr") (ENum 10) )





-- type Env = M.Map Id Value



-- (4 + 12) - 5
e0 :: Expr
e0 = EBin Sub (EBin Add (ENum 4) (ENum 12)) (ENum 5)

-- >>> eval (("x", 2000) : env) e1
-- 2001

-- (x + 1)
e1 :: Expr
e1 = EBin Add (EVar "x") (ENum 1)

-- (4 + a) - 5
e2 :: Expr
e2 = EBin Sub (EBin Add (ENum 4) (EVar "a")) (ENum 5)

-- >>> eval [] ex_10
-- 100


eval :: Env -> Expr -> Value
eval env e = case e of
  EVar x        -> lookup x env
  ENum n        -> VInt n
  EBin o ex ex' -> evalOp o (eval env ex) (eval env ex')
  ELet x ex1 ex2 -> eval env' ex2
                   where
                       env' = (x := xval) : env
                       xval = eval env ex1
  ELam x body    -> VClosure x body env
  EApp eFun eArg -> eval (x := vArg : frozEnv) eBody
                    where
                      VClosure x eBody frozEnv = eval env eFun
                      vArg                     = eval env eArg



{-
                                -- env
let c = 1
in                              -- (c:=1: env)
  let inc = \x -> x + c
  in                            -- [inc := <x, x+c, (c:=1 : env)>, c:=1, env]
    let c = 100
    in                          -- [c := 100, inc := <x, x+c, (c:=1 : env)>, c:=1, env]
      inc 10
                                eval  [x:=10, c:=1 , env)] (x + c)

  let inc = \x -> x + c
  in
    let c = 1
    in
      inc 10


-}

ex_inc_c :: Expr
ex_inc_c =
  ELet "c" (ENum 1)
    (ELet "inc" (ELam "x" (EBin Add (EVar "x") (EVar "c")))
      (ELet "c" (ENum 100)
        (
          EApp (EVar "inc") (EVar "c")
        )
      )
    )

-- >>> eval [] ex_add
-- VInt 1130

-- >>> eval [] (ELam "x" (ELam "y" (EBin Add (EVar "x") (EVar "y"))))

-- >>> let env1 = ["add" := VClosure "x" (ELam "y" (EBin Add (EVar "x") (EVar "y"))) [] ]
-- >>> let v_add10 = VClosure "y" (EBin Add (EVar "x") (EVar "y")) ["x" := VInt 10]
-- >>> let env2 = "add10" := v_add10 : env1
-- >>> let v_add20 = VClosure "y" (EBin Add (EVar "x") (EVar "y")) ["x" := VInt 20]
-- >>> let env3 = "add20" := v_add20 : env2
-- >>> eval env3 (EApp (EVar "add10") (ENum 100))
-- >>> eval env3 (EApp (EVar "add20") (ENum 1000))
-- VInt 110
-- VInt 1020


ex_add :: Expr
ex_add =                                              -- []
  ELet "add" (ELam "x" (ELam "y" (EBin Add (EVar "x") (EVar "y"))))
    (                                                 -- env1
      ELet "add10" (EApp (EVar "add") (ENum 10))
        (                                             -- env2
          ELet "add20" (EApp (EVar "add") (ENum 20))
            (                                         -- env3
              EBin Add (EApp (EVar "add10") (ENum 100)) (EApp (EVar "add20") (ENum 1000))
            )
        )
    )

{-

let add = \x y -> x + y
    add10 = add 10
    twice = \f x -> f (f x)
in
    twice add10 100

-}

-- >>> eval [] ex_twice
-- VInt 120

ex_twice :: Expr
ex_twice =
  ELet "add" (ELam "x" (ELam "y" (EBin Add (EVar "x") (EVar "y"))))
    (
      ELet "add10" (EApp (EVar "add") (ENum 10))
        (
          ELet "twice" (ELam "f" (ELam "x" (EApp (EVar "f") (EApp (EVar "f") (EVar "x")))))
            (
              EApp (EApp (EVar "twice") (EVar "add10")) (ENum 100)
            )
        )
    )



-- >>> eval []  ex_inc_c
-- VInt 101



--  \x -> x + 1
ex_inc_fun :: Expr
ex_inc_fun = ELam "x" (EBin Add (EVar "x") (ENum 1))

ex_incr' :: Expr
ex_incr' = ELet "incr" ex_inc_fun
            (EApp (EVar "incr") (ENum 2))

-- let incr = (\x -> x + 1) in incr 26

-- >>> eval [] ex_incr'
-- VInt 3

{-

  eval env (e1 e2)

  eval env e1 ===> VFun "x" eBody

  eval env e2 ===> v2

  eval ["x" := v2] eBody


-}



{-

  let x = 10
  in
      let x = x + 1
      in
          x




            -- env
let x = e1
in          -- eval ( (x := (eval env e1)) : env ) e2
    e2

let x = 0
in
            -- [ "x" := 0, ...]
  (x + 1)

let x = 0
in
  let y = 100
  in            -- [ "y" := 100, "x" := 0, ...]
    x + y


-}

{-
              -- ENV
let x = 10
in            -- (x := 10) : ENV
    x * y


    eval env expr




                              -- [ (x := 100) ]
let y = (let x = 2 in x) + x
in                            -- [ (y := 102), (x := 100)]
  let x = 3
  in                          -- [ (x := 3), (y := 102), (x := 100)]
    x + y

== 105



  let x = 3
  in
    x + ((let x = 2 in x) + x)







-}

ex_10 :: Expr
ex_10 = ELet "x" (ENum 10)
          (EBin Mul (EVar "x") (EVar "y"))

-- >>> eval [("y" := 2)] ex_10
-- 20






lookup :: Id -> Env -> Value
lookup x env = case env of
  []         -> error ("Variable not found: " ++ x)
  (y := v) : t -> if x == y then v else lookup x t


data Bind = Id := Value deriving (Show)

-- envA :: Env
-- envA = []

-- envB :: [Bind]
-- envB = ["x" := 0 , "y" := 9]

-- envC :: [Bind]
-- envC = ["x" := 9 , "y" := 0]

-- envD :: [Bind]
-- envD = ["x" := 9 , "y" := 10 , "z" := 666]

-- envE :: [Bind]
-- envE = [ "y" := 10, "x" := 42, "z" := 666, "x" := 99  ]






-- e1 == (x + 1)

-- >>> eval envE e1
-- 43


{-
env := [ "x" := 0, "y" := 12, ...]

eval env (x + 1)
    ==> (eval env x) + (eval env 1)
    ==> 0 + 1
    ==> 1

-}

-- >>> (\x -> x + 1) - 6


evalOp :: BOp -> Value -> Value -> Value
evalOp Add (VInt v1) (VInt v2) = VInt (v1 + v2)
evalOp Sub (VInt v1) (VInt v2) = VInt (v1 - v2)
evalOp Mul (VInt v1) (VInt v2) = VInt (v1 * v2)
evalOp _ _ _ = error "Oh no, a type error!"

{-
  What is the type of `evalOp` ?

        evalOp :: BOp -> Value -> Value -> Value

-}

-- >>> eval expr
-- 11

bob :: Int
bob = let ten = 10
      in
        let x = ten * ten
        in
           x * 3

eBob :: Expr
eBob = ELet "ten" (ENum 10)
          (ELet "x" (EBin Mul (EVar "ten") (EVar "ten"))
            (EBin Mul (EVar "x") (ENum 3)))

-- >>> eval [] eBob
-- 300


bob' :: [Int]
bob' = [ x * 3, x * 4 , x * 5]
  where
    x   = ten * ten
    ten = 10

{-

    ((10 - 2) - 2)


    (10 - (2 - 2))

    (1 + (2 * 3))

    ((1 + 2) * 3)

    1 + 2 * 3

    EBin Add (ENum 1) (EBin Add (ENum 2) (ENum 3))

    EBin Mul (EBin Add (ENum 1) (ENum 2) (ENum 3))

-}

-- >>> dummy
-- 101

dummy :: Int
dummy =             -- []
    let x = 0
    in              -- ["x" := 0]
      let x = 100
      in            -- ["x" := 100, "x" := 0]
        x + 1

crummy :: Int
crummy =            -- []
  let x = 0
  in                    -- ["x" := 0]
    (let x = 100
     in                     -- ["x" := 100, "x" := 0]
       x + 1
    )
    +                   -- ["x" := 0]
    x

{-

   [1,3,3,3]
     [4,3,2]
----------
   [6,6,6]
 [9,9,9,0]


pipe :: [a -> a] -> (a -> a)

pipe [] = ???
pipe fs = foldl' f base fs
  where
    f acc elem = elem acc
    base       = first (reverse fs)

foldl' op b [x1,x2,x3]
  ==> foldl' op (op b x1) [x2, x3]
  ==> foldl' op (op (op b x1) x2) [x3]
  ==> foldl' op (op (op (op b x1) x2) x3) []

  ==> (op (op (op b f1) f2) f3)

  base :: a -> a


pipe [f1,f2,f3] = foldl' f f3 [f1,f2,f3]

fs   = [f1,f2,f3]
base = f3

-}
