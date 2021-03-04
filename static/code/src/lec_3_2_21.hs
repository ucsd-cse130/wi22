{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{- LANGUAGE ExtendedDefaultRules #-}

module Lec_3_2_21 where 

import Prelude hiding (showList)


-- >>> 2 + 3

-- >>> 2.1 + 3.2
-- 5.300000000000001

-- >>> True + False


-- >>> 2 == 3
-- False

-- >>> 2 < 3
-- True

-- >>> True + True
-- No instance for (Num Bool) arising from a use of ‘+’


-- >>> ['c', 'a', 't'] == ['c', 'a', 't']
-- True

-- >>> ("cat", 2) < ("cat", 3)
-- True
















class JEq a where
    (===) :: a -> a -> Bool
    (===) x y = not (x =!= y)

    (=!=) :: a -> a -> Bool
    (=!=) x y = not (x === y)

-- A implements Eq then A implement JEq
instance (Eq a) => JEq a where
    (===) x y = x == y

-- >>> twelve =!= twelve
-- False

twelve :: Int
twelve = 12

data Days = Mon | Tue | Wed | Thu | Fri
            deriving (Eq, Ord, Show)

-- instance Show Days where
--   show Mon = "Mon" 
--   show Tue = "Tue" 
--   show Wed = "Wed" 
--   show Thu = "Thu" 
--   show Fri = "Fri" 

-- instance Eq Days where
--   (==) Mon Mon = True 
--   (==) Tue Tue = True 
--   (==) Wed Wed = True 
--   (==) Thu Thu = True 
--   (==) Fri Fri = True 
--   (==) _   _   = False 

-- >>> Fri < Fri 
-- False

class MyLess a where
    leq :: a -> a -> Bool

instance MyLess Int where
    leq i j = i <= j
instance MyLess String where
    leq i j = i <= j

instance (MyLess a, MyLess b) => MyLess (a, b) where
    leq (x1,y1) (x2,y2) 
      | leq x1 x2              = True 
      | leq x1 x2 && leq x2 x1 = leq y1 y2
      | otherwise              = False

-- >>> leq ("dat", 2::Int) ("cat", 3)
-- False


{- 

"DeriveGeneric"
"DeriveVia"

data Jhala = 

    GENERIC 

instance NEWCLASS Gen where 
    ...

 -}






-- Ambiguous type variable ‘a0’ arising from the literal ‘12’
-- prevents the constraint ‘(Num a0)’ from being solved.
-- Probable fix: use a type annotation to specify what ‘a0’ should be.
-- These potential instances exist:
--   instance Num a => Num (Blind a)
--     -- Defined in ‘Test.QuickCheck.Modifiers’
--   instance Num a => Num (Fixed a)
--     -- Defined in ‘Test.QuickCheck.Modifiers’
--   instance Num a => Num (Large a)
--     -- Defined in ‘Test.QuickCheck.Modifiers’
--   ...plus 80 others
--   (use -fprint-potential-instances to see them all)
-- No instance for (JEq a0) arising from a use of ‘===’











-- -- inc :: Int -> Int 
-- inc :: (Num a) => a -> a
-- inc x = x + 1

-- bloop = inc 12.2

-- ------------------------------

-- data Invisible = This | That | Other 

-- invisibleThings = [This, That, Other]

-- instance Show Invisible where
--     show This  = "*Inv: This*"
--     show That  = "*Inv: That*"
--     show Other = "*Inv: Other*"

-- instance Eq Invisible where
--     This  == This = True
--     That  == That = True
--     Other ==  Other = True
--     _     ==  _     = False

-- instance Ord Invisible where
--     x <= y = (show x) <= (show y)

-- data Env k v
--   = Def  v              -- default value `v` to be used for "missing" keys
--   | Bind k v (Env k v)  -- bind key `k` to the value `v`
--   deriving (Show)


-- add :: (Ord k) => k -> v -> Env k v -> Env k v  
-- add key val (Bind k v rest) 
--   | key < k         = Bind key val (Bind k v rest)
--   | otherwise       = Bind k v ( add key val rest )
-- add key val (Def v) = Bind key val (Def v)

-- get :: (Eq k) => k -> Env k v -> v  
-- get key (Bind k val rest) 
--   | key == k       = val
--   | otherwise      = get key rest 
-- get key (Def val)  = val

-- -- >>> add 'a' 50.0 (Def 0.0)
-- -- Bind 'a' 50.0 (Def 0.0)
-- --

-- -- >>> add 'c' 20.0 (Bind 'a' 50.0 (Bind 'b' 15.0 (Def 0.0)))
-- -- Bind 'a' 50.0 (Bind 'b' 15.0 (Bind 'c' 20.0 (Def 0.0)))
-- --



-- env0 = add 'f' 10.0 (add 'c' 20.0 (add 'a' 50.0 (Def 0.0)))

-- -- >>> get 'b' env0

-- -- >>> get "dog" env0
-- -- 20.0
-- --

-- -- >>> get "horse" env0



-- -- inc :: Int -> Int 
-- inc :: (Num a) => a -> a
-- inc x = x + 1

-- bloop = inc 12.2

-- -- >>> showList [1, 2, 3]
-- -- ["1","2","3"]
-- --

-- showList :: [Int] -> [String]
-- showList = mapList show

-- -- showList        :: [Int] -> [String]
-- -- showList []     =  []
-- -- showList (n:ns) =  show n : showList ns

-- -- >>> sqrList [1, 2, 3]
-- -- [1,4,9]
-- --

-- sqrList :: [Int] -> [Int]
-- sqrList = mapList (\n -> n ^ 2)
-- -- sqrList        :: [Int] -> [Int]
-- -- sqrList []     =  []
-- -- sqrList (n:ns) =  n^2 : sqrList ns

-- ----
-- data Tree a
--   = Leaf
--   | Node a (Tree a) (Tree a)
--   deriving (Show, Functor)

-- {- 
--       2 
--     /  \
--     1  3
--    / \ /\ 
--    L L L L 

-- -}

-- showTree :: Tree Int -> Tree String
-- showTree Leaf         = Leaf 
-- showTree (Node x l r) = Node x' l' r' 
--   where 
--     x' = (show x) 
--     l' = (showTree l) 
--     r' = (showTree r)

-- sqrTree :: Tree Int -> Tree Int
-- sqrTree Leaf         = Leaf 
-- sqrTree (Node x l r) = Node x' l' r' 
--   where 
--     x' = (x^2) 
--     l' = (sqrTree l) 
--     r' = (sqrTree r)



-- mapList :: (a -> b) -> [a] -> [b]
-- mapList f []     = [] 
-- mapList f (x:xs) = f x : mapList f xs

-- mapTree :: (a -> b) -> Tree a -> Tree b 
-- mapTree f Leaf         = Leaf 
-- mapTree f (Node x l r) = Node x' l' r' 
--   where 
--     x' = (f x) 
--     l' = (mapTree f l) 
--     r' = (mapTree f r)


-- -- mapList :: (a -> b) -> List a -> List b

-- -- mapTree :: (a -> b) -> Tree a -> Tree b 

-- class Mappable thing where
--   mapper :: (a -> b) -> thing a -> thing b  

-- {-
-- class Functor thing where
--   fmap :: (a -> b) -> thing a -> thing b  
--  -}


-- instance Mappable [] where 
--   mapper = mapList 

-- instance Mappable Tree where 
--   mapper = mapTree

-- -- Functor 

-- showThing :: (Functor thing) => thing Int -> thing String
-- showThing t = fmap show t

-- sqrThing :: (Functor thing) => thing Int -> thing Int
-- sqrThing t = fmap (\n -> n ^ 2) t



-- -- >>> showThing [1, 2, 3]
-- -- ["1","2","3"]
-- --

-- -- >>> showThing (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
-- -- Node "2" (Node "1" Leaf Leaf) (Node "3" Leaf Leaf)
-- --

-- -- >>> sqrThing [1,2,3,4]
-- -- [1,4,9,16]
-- --


-- -- >>> sqrThing (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
-- -- Node 4 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf)
-- --


-- ----

-- data Expr
--   = Number Int
--   | Plus   Expr Expr
--   | Div    Expr Expr
--   deriving (Show)

-- data Result v 
--   = ErrorMsg String 
--   | Value v  
--   deriving (Show)

-- eval :: Expr -> Result Int
-- eval (Number n)   = Value n
-- eval (Plus e1 e2) = do n1 <- eval e1
--                        n2 <- eval e2
--                        Value (n1 + n2)

-- eval (Div  e1 e2) = do n1 <- eval e1
--                        n2 <- eval e2
--                        if n2 == 0 
--                           then ErrorMsg ("YIX: dbz due to " ++ show e2)  
--                           else Value (n1 `div` n2)

-- instance Functor Result where
--   fmap _ (ErrorMsg e) = ErrorMsg e
--   fmap f (Value v)    = Value (f v)

-- instance Applicative Result where
--   pure x = Value x

-- instance Monad Result where 
--   (>>=) = foo


-- {- 

-- e1 >>= \n1 -> 
--   e2 >>= \n2 -> 
--     e3 >>= \n3 -> 
--       STUFF

-- do n1 <- e1
--    n2 <- e2
--    n3 <- e3
--    STUFF

-- -}


-- foo :: (Result a) -> (a -> Result b) -> Result b
-- foo e f =  
--   case e of 
--     ErrorMsg err -> ErrorMsg err
--     Value val    -> f val 


-- -- >>> eval (Div (Number 6) (Number 2))
-- -- Value 3
-- --

-- -- >>> eval (Div (Number 6) (Number 0))
-- -- ErrorMsg "YIX: dbz due to Number 0"
-- --

