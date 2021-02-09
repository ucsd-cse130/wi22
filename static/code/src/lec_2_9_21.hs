{-# LANGUAGE PartialTypeSignatures #-}

module Lec_2_9_21 where

import Text.Printf

import Debug.Trace

-- fac :: Int -> Int
-- fac n 
--   | n <= 0    = 1
--   | otherwise = n * fac (n-1)

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

fibb :: Int -> Int
fibb n = if n <= 1 then 1 else fibb (n-1) + fibb (n-2)

fib :: Int -> Int 
fib n = loop 1 0 1 
  where
    loop i a b
      | i <= n    = loop (i+1) b (a+b)
      | otherwise = b

fac :: Int -> Int
fac n = loop 1 1 
  where
   loop i res 
     | i <= n    = loop (i + 1) (res * i)
     | otherwise = res

{- fac 4 ==> 
   loop 1 1 => 
   loop (1+1) (1*1) => 
   loop (1+1+1) (1*1*2) => 
   loop 4 (1*2*3) => 
   loop 5 (1*2*3*4) => 24
 -}
