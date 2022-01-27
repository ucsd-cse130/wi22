module Lec_1_20_22 where

sumTo :: Int -> Int
-- sumTo n = if n <= 0 then 0 else n + sumTo (n-1)

-- sumTo 0 = 0
-- sumTo n = n + sumTo (n-1)

sumTo n 
  | n <= 0    = 0 
  | otherwise = n + sumTo (n-1)

charA :: Char
charA = 'a'

pair0 :: (Char, Int)
pair0 = (charA, 22)

pair1 :: (Char, Integer, Double)
pair1 = (charA, 22, 5.6)

fst3 :: (a, b, c) -> a
fst3 tup = case tup of
             (v, _, _) -> v

snd3 :: (a, b, c) -> b
snd3 tup = case tup of
             (_, v, _) -> v

thd3 :: (a, b, c) -> c
thd3 tup = case tup of
             (_, _, v) -> v

-- >>> thd3 pair1 
-- 5.6


-- oops :: [Int]
-- oops = [1, 2, 'c']

sillyList :: [Integer]
sillyList = [1, 2, 3, 4]

sillyList' :: [Integer]
sillyList' = 1 : (2 : (3 : (4 : [])))

-- >>> sillyList == sillyList'
-- True




tup3 :: ((Integer, Double), Bool)
tup3 = ((7, 5.2), True)

tup3' :: (Integer, (Double, Bool))
tup3' = (7, (5.2, True))


{- 
PAIR 
FST
SND 

-}
-- >>> tup3 == tup3' 
-- Couldn't match type ‘Integer’ with ‘(Integer, Double)’
-- Expected type: ((Integer, Double), Bool)
--   Actual type: (Integer, (Double, Bool))

-- (A) True
-- (B) False
-- (C) I will not answer that question! (compile-time-error)
-- (D) Crash! (run-time error)




-- >>> otherwise
-- True


{- EXAMPLE OF SUBSTITUTING EQUALS FOR EQUALS  

sumTo 3
== 5 + sumTo (5 - 1)
== 3 + sumTo 2
== 3 + 2 + sumTo 1
== 3 + 2 + 1 + sumTo 0
== 3 + 2 + 1 + 0
== 6

-}



-- >>> copy3 "cat" 
-- ["cat","cat","cat"]

-- >>> copy3 0    
-- [0,0,0]

copy3 :: a -> [a]
copy3 x = [x, x, x]


-- >>> clone 0 "cat" 
-- []

-- >>> clone 1 "cat" 
-- ["cat"]

-- >>> clone 2 "cat" 
-- >>> clone 3 "dog" 
-- ["dog","dog","dog"]

-- >>> clone 4 100
-- [100,100,100,100]

-- clone :: Int -> a -> [a]
clone :: Int -> a -> [a]
clone n cat 
  | n <= 0    = [] 
  | otherwise = cat:clone (n-1) cat

-- clone 3 "dog" ==> "dog" : "dog" : dog : [] 

-- RANGE 

-- ENGLISH: range i j == [i, i+1, ... , j]

-- >>> range 0 3 
-- /Users/rjhala/teaching/130-wi22/static/code/src/lec_1_20_22.hs:(136,1)-(139,19): Non-exhaustive patterns in function range

-- TESTS
range :: Int -> Int -> [Int] 
range i j 
  | i <= j    = i : range (i+1) j
  | otherwise = []

{- 

  range 0 3
  => 0 : (range 1 3)
  => 0 : (1 : range 2 3)
  => 0 : (1 : (2 : range 3 3))
  => 0 : (1 : (2 : 3 : range 4 3))
  => 0 : (1 : (2 : 3 : [] ))

-}


{- 

[e1,e2,e3,e4] == e1:e2:e3:e4:[]


Is 
    [ 10 ]

the same as 

     10:[] 




-}


firstElem :: [Int] -> Int 
firstElem [] = 0
firstElem (x:_) = x

-- firstElem xs = case xs of
--   [] -> 0
--   h : _ -> h

{- -}

-- -- SOL 
-- firstElem :: [Int] -> Int
-- firstElem list = case list of
--     [] -> 0 
--     x1:_  -> x1

-- mystery l = case l of
--   [] -> 0
--   _ : as -> 1 + mystery as

mystery :: [Int] -> Int 
mystery []      = 0
mystery (a: as) = a + mystery as

{- 
foo bar baz 

((foo bar) baz)



(mystery _) : as

mystery (_ : as)

-}

-- >>> mystery [10, 20, 30]
-- 60

-- mys (10: 20: 30 : [])
-- => 10 + mys (20: 30 : []) 
-- => 10 + (20 + mys (30 : []) )
-- => 10 + (20 + (30 + (mys [])))
-- => 10 + (20 + (30 + (0))))
-- => 60

data Day = Mon | Tue | Sun

dayToInt :: Day -> Int
dayToInt d = case d of
  Mon -> 0
  Tue -> 1
  Sun -> 2
