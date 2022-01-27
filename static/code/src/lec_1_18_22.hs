module Lec_1_18_22 where


pat :: Integer -> Integer -> Integer -> Integer
-- pat       = \x -> \y -> \z -> x * (y + z)
-- pat x     = \y -> \z -> x * (y + z)
-- pat x y   = \z -> x * (y + z)
-- pat x y z = x * (y + z)

-- >>> pat 10 20 30
-- 500

pat x y z = x * (y + z)

weirdo :: Int
weirdo = inc 100 

-- this is a comment
{- this is also a comment -}

-- >>> 2 + 2 
-- 4

-- weirdo = inc 100 = 100 + 1 = 101
-- >>> weirdo
-- 101



-- >>> pat 90 68 12 
-- 7200



inc :: Num a => a -> a
inc x = x + 1

-- >>> inc 10.5 
-- 11.5

exBool :: Bool
exBool = True

exChar :: Char
exChar = 'c'


ex1 :: Bool
ex1 = True

ex2 :: Int
ex2 = 4 + 5

ex3 :: Double
ex3 = 4.1 + 5.2

-- quiz1 :: (Ord a, Num a) => a -> a -> Bool
quiz1 :: Int -> Int -> Bool
quiz1 x y = (x + y) > 0 

{- 
(A) Int     (B) Bool    (C) "Num" (D) "Fractional"  (E) Error!


Env |- e1 :: Bool       Env |- e2 :: T      Env |- e3 :: T
-----------------------------------------------------------
    Env |- if e1 then e2 else e3 :: T

-}

quiz = if True then 4.3 else 4.1

isPos :: Int -> Bool
isPos n = n > 0

-- >>> isPos (2+4)
-- True

-- >>> isPos (2-4)
-- False


-- isPos (2 + 4) == isPos 6 == 6 > 0 == True

-- isPos (2 - 4) == isPos (-2) == -2 > 0 == False

adder :: Int -> Int -> Int
adder = \x -> \y -> x + y


bloop :: Int -> Int
bloop = adder 10

-- >>> adder 10 20 == 10 + 20 == 30

-- >>> ((adder 10) 20) == ((\y -> 10 + y) 20) == 10 + 20 == 30  

-- "currying"

myMax :: Int -> Int -> Int
myMax x y = if x > y then x else y

-- >>> myMax 100 1
-- 100

sumTo :: Int -> Int
-- sumTo 0 = 0
-- sumTo n = n + sumTo (n-1)

-- sumTo n = if n <= 0 then 0 else n + sumTo (n-1)

{- EXAMPLE OF SUBSTITUTING EQUALS FOR EQUALS  

sumTo 5
== 5 + sumTo (5 - 1)
== 5 + sumTo 4
== 5 + (4 + sumTo (4-1))
== 5 + (4 + sumTo 3)
== 5 + (4 + (3 + sumTo (3-1)))
== 5 + (4 + (3 + sumTo 2))
== 5 + (4 + (3 + (2 + sumTo (2-1))))
== 5 + (4 + (3 + (2 + sumTo 1)))
== 5 + (4 + (3 + (2 + (1 + sumTo (1 - 1))))
== 5 + (4 + (3 + (2 + (1 + sumTo 0))))
== 5 + (4 + (3 + (2 + (1 + 0))))
== ... == 15

-}

sumTo n 
  | n <= 0    = 0 
  | otherwise = n + sumTo (n-1)

-- >>> sumTo 3
-- 6

-- >>> sumTo 1 
-- /Users/rjhala/teaching/130-wi22/static/code/src/lec_1_18_22.hs:107:1-11: Non-exhaustive patterns in function sumTo
