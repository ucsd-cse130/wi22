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
data Expr 
    = ENum Int
    | EBin BOp Expr Expr
    | EVar Id
    | ELet Id  Expr Expr 
    deriving (Show)




type Id = String

type Value = Int

data BOp = Add | Sub | Mul deriving (Show)

type Env = [Bind]

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

eval :: Env -> Expr -> Value
eval env e = case e of  
  EVar x        -> lookup x env
  ENum n        -> n
  EBin o ex ex' -> evalOp o (eval env ex) (eval env ex')
  ELet x e1 e2  -> eval env' e2
                   where 
                       env' = (x := xval) : env
                       xval = eval env e1
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




lookup :: Id -> Env -> Value
lookup x env = case env of
  []         -> error ("variable not found: " ++ x) 
  (y := v) : t -> if x == y then v else lookup x t


data Bind = Id := Value

envA :: Env
envA = []
envB :: [Bind]
envB = ["x" := 0 , "y" := 9]
envC :: [Bind]
envC = ["x" := 9 , "y" := 0]
envD :: [Bind]
envD = ["x" := 9 , "y" := 10 , "z" := 666]
envE :: [Bind]
envE = [ "y" := 10, "x" := 42, "z" := 666, "x" := 99  ]






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


evalOp :: BOp -> Value -> Value -> Value
evalOp o v1 v2 = case o of
  Add -> v1 + v2 
  Sub -> v1 - v2 
  Mul -> v1 * v2 

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
