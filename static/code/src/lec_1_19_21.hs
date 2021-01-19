{-# LANGUAGE PartialTypeSignatures #-}

module Lec_1_19_21 where


-- this is a comment! 

ex1 :: Int
ex1 = 31 * (42 + 56)


ex2 :: Double
ex2 = 3 * (4.2 + 5.6) 

ex3 :: Char
ex3 = 'a'

ex4 :: Bool
ex4 = True

ex5 :: Bool
ex5 = False

----

ex6 :: Int
ex6 = 4 + 5

ex7 :: Int
ex7 = 4 * 5

ex8 :: Bool
ex8 = 5 > 4 

-- >>> ex8 
-- True

-- >>> quiz
-- 9

{-
david_quiz :: ???
david_quiz = if ex8 then ex6 else "chicago"

**A.** `Int`

**D.** ERROR

-}

--- 

{- oh this is bad -}

{- this is ALSO a comment -}

{- 
   10 * (12 + 134)
   54 * (19 + 7)
   98 * (34 + 9)
 -}

-- >>> 56 + 10
-- 66


isPos :: Int -> Bool
isPos n = n > 0 

-- isPos = \n -> n > 0

add3 :: Int -> Int -> Int -> Int
add3 x1 x2 x3 = x1 + x2 + x3


quiz :: Int -> Int -> Bool
quiz  = \x  -> \y  -> x + y > 0


-- (((add3 10) 20) 30)
-- >>> add3 10 20 30 
-- 60

-- add3 = \x1 x2 x3 -> x1 + x2 + x3
-- add3 x1 = \x2 x3 -> x1 + x2 + x3
-- add3 x1 x2 = \x3 -> x1 + x2 + x3


-- >>> isPos (10 + 2)
-- True

-- >>> isPos (10 - 20)
-- False

-- isPos (10 + 2) ==> isPos 12 ==> 12 > 0 ==> True
-- isPos (10 - 20) ==> isPos (-20) ==> -20 > 0 ==> False

pat :: Int -> Int -> Int -> Int 
pat = \x y z -> x  * ( y + z )

-- >>> (((pat 10) 20) 30) 
-- 500

--    (((\y z -> 10 * (y + z)) 20) 30) 
--    (((\z -> 10 * (20 + z))) 30) 
--    (((10 * (20 + 30)))) 
--    10 * 50 
--    500


myMax :: Int -> Int -> Int 
myMax x y = if x > y then x else y

-- >>> myMax 1000 20
-- 1000

-- sumTo n = 0 + 1 + 2 + 3 + ... + n
-- sumTo 0 = 0
-- sumTo 1 = 0 + 1
-- sumTo 2 = 0 + 1 + 2
-- sumTo 3 = 0 + 1 + 2 + 3

sumTo :: Int -> Int
sumTo n = if n == 0 then 0 else n + sumTo (n - 1) 

-- 
-- >>> sumTo 4
-- == sumTo 4 
-- ==> if 4 == 0 then 0 else 4 + sumTo (4 - 1) 
-- ==> if False then 0 else 4 + sumTo (4 - 1) 
-- ==> 4  + sumTo (4 - 1) 
-- ==> 4  + sumTo 3
-- ==> 4  + 3 + sumTo 2
-- ==> 4  + 3 + 3 + sumTo 1
-- ==> 4  + 3 + 2 + 1 + sumTo 0
-- ==> 4  + 3 + 2 + 1 + 0
-- ==> 10

