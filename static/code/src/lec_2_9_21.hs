{-# LANGUAGE PartialTypeSignatures #-}

module Lec_2_9_21 where

import Text.Printf

import Debug.Trace
import Data.Char (toUpper)

fac :: Int -> Int
-- fac n 
--   | n <= 0    = 1
--   | otherwise = n * fac (n-1)

-- >>> fac 10
-- 3628800



silly = let x = 2 
            y = 3
        in
            x + y 

billy = x + y 
  where 
    x = 2 
    y = 3

{- 
CAN BE COMPILED DOWN TO EXACTLY THE SAME CODE AS:

int result = 1
for int i = 0; i < n; i++ {
  result * (i + 1)
}
-}

{-
 fac 5
 => loop 1 1 
 => loop 1 2
 => loop 2 3
 => loop 6 4
 => loop (24) 5
 => loop (120) 6
 => 120

-}


{- 
def fac(n):
  res = 1
  i   = 1
  while (i <= n):
    res, i = res * i, i + 1 
  return res

n = 5 
res = 1, i = 1
res = 1 * 1, i = 2
res = 1 * 1 * 2, i = 3
res = 1 * 1 * 2 * 3 , i = 4
res = 1 * 1 * 2 * 3 * 4 , i = 5
res = 1 * 1 * 2 * 3 * 4 * 5 , i = 6
 -}


-- >>> fac 5
-- 120

{- 
fac 5 
==> 5 * fac 4
==> 5 * (4 * fac 3)
==> 5 * (4 * (3 * fac 2))
==> 5 * (4 * (3 * (2 * fac 1)))
==> 5 * (4 * (3 * (2 * (1 * fac 0))))
==> 5 * (4 * (3 * (2 * (1 * 1))))
==> 5 * (4 * (3 * (2 * 1))
==> 5 * (4 * (3 * 2))
==> 5 * (4 * 6)
==> 5 * (24)
==> 120

-}




size :: [a] -> Int
size xs = loop 0 xs
  where
    loop acc []    = acc  
    loop acc (_:t) = loop (acc+1) t

{- 
size ['a', 'b', 'c']
=> loop 0 ('a': ['b', 'c'])
=> loop 1 [ 'b', 'c']
=> loop 2 [ 'c']
=> loop 3 [] 
=> 3 


-}

fac n = loop 1 1 
  where 
   loop acc i 
     | i <= n    = loop (acc * i) (i + 1)
     | otherwise = acc






{- 
def fac(n):
  res = 1
  i   = 1
  while (i <= n):
    res = res * i
    i   = i + 1 
  return res

def fib(n):
  a,b = 0,1
  i   = 1
  while (i <= n):
    a, b = b, a+b 
    i = i + 1
  return b

-}

-- fibb :: Int -> Int
-- fibb n = if n <= 1 then 1 else fibb (n-1) + fibb (n-2)

-- fib :: Int -> Int 
-- fib n = loop 1 0 1 
--   where
--     loop i a b
--       | i <= n    = loop (i+1) b (a+b)
--       | otherwise = b

-- fac :: Int -> Int
-- fac n = loop 1 1 
--   where
--    loop i res 
--      | i <= n    = loop (i + 1) (res * i)
--      | otherwise = res

-- {- fac 4 ==> 
--    loop 1 1 => 
--    loop (1+1) (1*1) => 
--    loop (1+1+1) (1*1*2) => 
--    loop 4 (1*2*3) => 
--    loop 5 (1*2*3*4) => 24
--  -}


-- >>> evens [1,2,3,4]
-- [2,4]

evens :: [Int] -> [Int]
evens xs     = foo isEven xs 
-- evens []      = [] 
-- evens (x:xs) 
--   | even x  = x : rest 
--   | otherwise = rest
--   where
--     rest      = evens xs 



fourChars xs = foo isFour xs

inc :: Num a => a -> a
inc x = x + 1

binc :: Integer -> Integer
binc = inc



fourChars :: [String] -> [String]
-- fourChars []    = [] 
-- fourChars (x:xs) 
--   | size x == 4 = x : rest 
--   | otherwise   = rest
--   where
--     rest        = fourChars xs

isFour x = size x == 4

isEven x = x `mod` 2 == 0



foo _    []   = [] 
foo cond (x:xs) 
  | cond x    = x : rest 
  | otherwise = rest
  where
    rest      = foo cond xs
{- 

 

foo []        = [] 
foo (x:xs) 
  | cond      = x : rest 
  | otherwise = rest
  where
    rest      = foo xs 
    cond      = even x

foo []        = [] 
foo (x:xs) 
  | cond      = x : rest 
  | otherwise = rest
  where
    rest        = foo xs
    cond        = size x == 4

-}
-- isEven :: Int -> Bool
-- isEven x = x `mod` 2 == 0


-- >>> fourChars ["i", "must", "do", "work"]
-- ["must","work"]


-- shout [] = []
-- shout ('h' : ['e', 'l', 'l', 'o']) = 'H' : ['E', 'L', 'L', 'O']

-- >>> shout "hello"
-- "HELLO"

shout :: [Char] -> [Char]
-- shout []     = [] 
-- shout (x:xs) = toUpper x : shout xs
shout = map toUpper 

-- >>> squares [1,2,3,4,5,6]
-- [1, 4, 9, 16, 25, 36]

squares :: [Int] -> [Int]
-- squares []     = []
-- squares (x:xs) = (x * x) : squares xs
squares xs = map (\x -> x * x) xs

sizes = map size

-- >>> sizes ["this", "is", "the", "end", "my", "friend"]
-- [4,2,3,3,2,6]

map' :: (t -> a) -> [t] -> [a]
map' f []     = [] 
map' f (x:xs) = f x : map' f xs








