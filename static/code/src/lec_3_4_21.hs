{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Lec_3_2_21 where 

import Prelude hiding (showList)
import Numeric


-- >>> "0x" ++ (showHex 170 "") 
-- "0xaa"


-- +, - 
-- ==, <= 
-- show

{- 
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

class Ord a where
    (<=) :: a -> a -> Bool
    (>)  :: a -> a -> Bool

class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a

class Show a where
    show :: a -> String 

 -}

data SMod2 = SZero | SOne deriving (Show)

instance Num SMod2 where
    (+) SZero SZero = SZero 
    (+) SZero SOne  = SOne
    (+) SOne  SZero = SOne
    (+) SOne  SOne  = SZero 

instance Num Bool where
    (+) True True = False
    (+) True False = True 
    (+) False True = True
    (+) False False = False 


plus :: Num a => a -> a -> a
plus x y = x + y

{- 

JSON = Javascript Object Notation

(A) have heard of
(B) say what?

-}
-- >>> plus False False 
-- False

{- 


(A) plus :: Int -> Int -> Int
(B) plus :: SMod2 -> SMod2 -> SMod2 
(C) plus :: a -> a -> a
(D) plus :: (Num a) => a -> a -> a
(E) plus :: (Eq a) => a -> a -> a

 -}


-- >>> SZero + SZero
-- SZero



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


instance ToJSON a => ToJSON (Env String a) where
    toJson env = JObj (helper env)

helper :: (ToJSON a) => Env String a -> [(String, JVal)]
helper (Def v)         = [("def", toJson v)]
helper (Bind k v rest) = (k, toJson v) : helper rest

-- >>> toJson env0
-- JObj [("cat",JNum 10.0),("dog",JNum 20.0),("def",JNum 0.0)]

data Env k v
  = Def  v              -- default value `v` to be used for "missing" keys
  | Bind k v (Env k v)  -- bind key `k` to the value `v`
  deriving (Show, Functor)

env0 :: Env String Double
env0 = set "cat" 10.0 (set "dog" 20.0 (Def 0))

set :: k -> v -> Env k v -> Env k v
set k v env = Bind k v env


get :: Eq k => k -> Env k v -> v 
get key (Def val)       = val
get key (Bind k v rest) 
  | k == key            = v
  | otherwise           = get key rest

-- >>> get "cat" env0
-- 10

-- >>> get "dog" env0
-- 20

-- >>> get "horse" env0
-- 0

data JVal
  = JStr  String
  | JNum  Double
  | JBool Bool
  | JObj  [(String, JVal)]
  | JArr  [JVal]
  deriving (Eq, Ord, Show)

lunches = [ [("day", "monday"),    ("loc", "zanzibar")]
          , [("day", "tuesday"),   ("loc", "farmers market")]
          ]

-- >>> toJson lunches 
-- JArr [JObj [("day",JStr "monday"),("loc",JStr "zanzibar")],JObj [("day",JStr "tuesday"),("loc",JStr "farmers market")]]

instance (ToJSON a) => ToJSON [(String, a)] where
  toJson kvs = JObj [ (k, toJson v) | (k, v) <- kvs ] 

instance ToJSON String where
  toJson s = JStr s

instance ToJSON Double where
  toJson d = JNum d

instance ToJSON a => ToJSON [a] where
    toJson xs = JArr (map toJson xs)

class ToJSON a where
    toJson :: a -> JVal



-- >>> toJson [["Ranjit", "Jhala"], ["cat", "horse"]]
-- JArr [JArr [JStr "Ranjit",JStr "Jhala"],JArr [JStr "cat",JStr "horse"]]

-- >>> toJson [45.1 :: Double, 23.0, 99.0]
-- JArr [JNum 45.1,JNum 23.0,JNum 99.0]


hs1 = 
    ( ("name", "Ranjit")
    , ("age", 41.0)
    , ("likes", ["guac", "coffee", "bacon"])
    , ("silly", [45.1, 23.0, 99])
    )

js1 :: JVal
js1 =
  JObj [("name", JStr "Ranjit")
       ,("age",  JNum 41.0)
       ,("likes",   JArr [ JStr "guacamole", JStr "coffee", JStr "bacon"])
       ,("hates",   JArr [ JStr "waiting"  , JStr "grapefruit"])
       ,("lunches", JArr [ JObj [("day",  JStr "monday")
                                ,("loc",  JStr "zanzibar")]
                         , JObj [("day",  JStr "tuesday")
                                ,("loc",  JStr "farmers market")]
                         , JObj [("day",  JStr "wednesday")
                                ,("loc",  JStr "hare krishna")]
                         , JObj [("day",  JStr "thursday")
                                ,("loc",  JStr "faculty club")]
                         , JObj [("day",  JStr "friday")
                                ,("loc",  JStr "coffee cart")]
                         ])
       ]













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


---------------------------------------------------

-- >>> showListt [1,2,3,4]
-- ["1","2","3","4"]

showListt :: [Int] -> [String]
showListt = mapList show
-- showListt []     =  []
-- showListt (n:ns) =  show n : showListt ns

-- >>> sqrList [1,2,3,4]
-- [1,4,9,16]

sqrList        :: [Int] -> [Int]
sqrList = mapList (\n -> n*n)
-- sqrList []     =  []
-- sqrList (n:ns) =  n * n : sqrList ns

mapList :: (a -> b) -> [a] -> [b]
mapList f []     =  []
mapList f (n:ns) =  f n : mapList f ns


showEnv :: Env k Double -> Env k String
showEnv (Def v)         = Def (show v)
showEnv (Bind k v rest) = Bind k (show v) (showEnv rest)

mulEnv :: Double -> Env k Double -> Env k Double
mulEnv s (Def v)         = Def (s * v)
mulEnv s (Bind k v rest) = Bind k (s * v) (mulEnv s rest)

mapEnv :: (a -> b) -> Env k a -> Env k b
mapEnv f (Def v)         = Def (f v)
mapEnv f (Bind k v rest) = Bind k (f v) (mapEnv f rest)

{- 

mapList :: (a -> b) -> [a] -> [b]

mapEnv  :: (a -> b) -> Env k a -> Env k b

-}

class Mappable t where
    mymap :: (a -> b) -> t a -> t b

instance Mappable [] where
   mymap = mapList 

instance Mappable (Env k) where
   mymap = mapEnv


example = fmap (** 2) [1,2,3,4,5]

-- >>> fmap (** 2) [1,2,3,4,5]
-- [1.0,4.0,9.0,16.0,25.0]

-- >>> fmap (* 0.5) env0 
-- Bind "cat" 5.0 (Bind "dog" 10.0 (Def 0.0))


-- Bind "cat" 20.0 (Bind "dog" 40.0 (Def 0.0))


readHex :: String -> Integer
readHex s = read s 

-- >>> readHex "0xAA"
-- 170

