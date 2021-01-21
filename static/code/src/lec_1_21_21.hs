{-# LANGUAGE PartialTypeSignatures #-}

module Lec_1_21_21 where

-- sumTo n = 0 + 1 + 2 + 3 + ... + n
-- sumTo 0 = 0
-- sumTo 1 = 0 + 1
-- sumTo 2 = 0 + 1 + 2
-- sumTo 3 = 0 + 1 + 2 + 3

thing1 :: Integer
thing1 = 10

thing2 :: Integer
thing2 = thing1 + thing1

sumTo :: Int -> Int
sumTo n = if n == 0 then 0 else n + sumTo (n - 1) 

dividr :: Int -> Int -> (Int, Int) 
dividr 10 3 = (3, 1) -- quotient 3, remainder = 1

-- MKPAIR 3 1
-- sumTo 0 = 0
-- sumTo n = sumTo (n-1) + n


tup1 :: (Integer, String)
tup1 = (12, "horse")

tup2 :: (Integer, String, Double)
tup2 = (12 + 78, "dog", 9.8)

tup3 :: ((Integer, Double), Double -> Double)
tup3 = ((7, 5.2) , \x -> x + 1)
         -- ((Int, Double)     ,  Int -> Int)
{- IF 
     e1 :: T1
     e2 :: T2
   THEN
     (e1, e2) :: (T1, T2)

   tup3 == ((7, 5.2), True) :: ((Integer, Double), Bool)
e1 == (7, 5.2) :: (Integer, Double) == T1
e2 == True     :: Bool              == T2
-}

ryanEx :: Double
ryanEx = two_eight + two 

two_eight :: Double
two_eight = 2.8

-- two :: Int
two = 2

-- 1. WRITE TESTS
--
-- getFst (10, 20) = 10
-- getFst ("cat", "horse") = "cat"

-- 2. WRITE TYP


-- >>> addPair (10, 20)
-- 30

-- >>> addPair (100, 2)
-- 102

addPair :: (Int, Int) -> Int
addPair = \t -> case t of (x1, x2) -> x1 + x2

add :: Int -> Int -> Int
add = \x1 -> \x2 -> x1 + x2


-- >>> addPair (10, 20)
-- Couldn't match expected type ‘Int’ with actual type ‘(a0, b0)’


-- >>> maxPair (10, 20)
-- 20

-- >>> maxPair (100, 20)
-- 100

maxPair :: (Int, Int) -> Int
maxPair (x1, x2) = if x1 > x2 then x1 else x2


getFirst :: (t1, t2) -> t1
getFirst param = case param of
                 (x1, x2) -> x1

getFirst' (x1, _) = x1

getFirst3 :: (t1, t2, t3) -> t1
getFirst3 (x1, x2, x3) = x1

-- >>> getFirst ("10", 20)
-- "10"
-- >>> getFirst ("10", 20, "boo")
-- Couldn't match expected type ‘(t1, t20)’
--             with actual type ‘([Char], b0, [Char])’


-- getFirst ("cat", 12)  ===> x1="cat", x2=12 -> "cat"
--
-- getSnd (10, 20) = 20





ints :: [Int]
ints = [1,2,3,4,5,6,7]

chars :: [Char]
chars = ['c', 'a', 't']

string :: [Char]
string = "cat"

-- >>> chars == "cat"
-- True

pairs :: [(Int, Bool)]
pairs = [(1, True), (2, False), (3, True)]

{- 

  TUPLE = fixed SIZE but different TYPES

  LIST = different SIZEs but fixed TYPE

-}
things :: [[Integer]]
things = [] -- [ [1], [2, 3], [4, 5, 6] ] 



oops = (1, 2, 'c')
{- 
 Int     = Fixed-size (32 or 64 bit) FAST but CAN overflow 
 Integer = Not-fixed size ... SLOW but CANNOT overflow

-}

{- 

IF 
  e1 :: T
  e2 :: T
  e3 :: T ...

THEN
  [e1, e2, e3,...] :: [T]
   

:: [T]
[ 
  e1 :: T 

, e2 :: T

, e3 :: T
]
[ 
  [1]   :: [Int]

, [2, 3]  :: [Int]

, [4, 5, 6] :: [Int] 

] 

-}



-- >>> 3 : []
-- [3]

-- >>> 2 : (3 : [])
-- [2,3]

-- >>> 1 : numbers 
-- [1,10,20,30

-- 1 : 10 : 20 : 30 : []



-- >>> numbers 
-- [10,20,30]

numbers = [10,20,30]

example = undefined : undefined

{- 

  (:) :: thing -> [thing] -> [thing]
>> 1
  h : t 
 -}


-- >>> copy3 "horse" 
-- ["horse","horse","horse"]

-- >>> copy3 12      
-- [12,12,12]

copy3 :: a -> [a]
copy3 x = [x,x,x]


clone :: Int -> a -> [a] 
-- clone n c = case n of 
--               0 -> [] 
--               _ -> c : clone (n-1) c

clone 0 c = []
clone n c = c : clone (n-1) c 

{- 
clone 3 "cat"
==> "cat" : clone 2 "cat" 
==> "cat" : "cat" : clone 1 "cat" 
==> "cat" : "cat" : "cat" : clone 0 "cat" 
==> "cat" : "cat" : "cat" : []


-}

firstElem :: [Int] -> Int
-- firstElem ns = case ns of
--                  []  -> 0 
--                  h:_ -> h

firstElem [] = 0
firstElem (h:_) = h

lastElem :: [Int] -> Int
-- lastElem []    = 0 
-- lastElem [x]   = x
-- lastElem (h:t) = lastElem t
lastElem l = case l of
               []  -> 0
               [x] -> x 
               _:t -> lastElem t 

mystery :: [Int] -> Int
mystery []     = 0
mystery (x:xs) = x + mystery xs


-- >>> mystery [10,20,30]
-- 60

-- mystery (10 : 20 : 30 : [])
-- ==> 10 + mystery (20 : 30 : [])
-- ==> 10 + 20 + mystery (30 : [])
-- ==> 10 + 20 + 30 + mystery []
-- ==> 10 + 20 + 30 + 0 
-- ==> 60

williamList :: [Int] 
williamList = [1,2] ++ [3,4]

{- 

  [1,2] : [3, 4] ==> [1,2,3,4]

  (:)   ::   a -> [a] -> [a]

-}
