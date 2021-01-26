{-# LANGUAGE PartialTypeSignatures #-}

module Lec_1_26_21 where

import Prelude hiding (take)

-- size [x1,x2,x3] = 3
size :: [a] -> Int
size []    = 0
size (_:t) = 1 + size t 


-- size xs = case xs of
--             []  -> 0
--             _:t -> 1 + size t 


-- range 

-- 1. Write Tests 
-- range lo hi == [lo, lo+1, lo+2, lo+3,...,hi]
-- range 4 3 == []
-- range 3 3 == [3]
-- range 2 3 == [2,3]
-- range 1 3 == [1,2,3]
-- range 0 3 == [0,1,2,3]

-- >>> range 0 3 
-- [0,1,2]

-- 2. Write Type of the function
range :: Int -> Int -> [Int] 
range lo hi 
  | lo >= hi  = [] 
  | otherwise = let lo' = lo + 1 
                    tl  = range lo' hi 
                in 
                    lo : tl 

{-  let x = e1 in 
      e2 
 -}


-- >>> otherwise
-- True


-- range 3 3 =       [3]
-- range 2 3 =     [2,3]
-- range 1 3 =   [1,2,3]
-- range 0 3 = 0 : range 1 3
-- range lo hi = lo : range (lo + 1) hi

-- 3. Write code

--------------------------------------------

-- take k [x1,x2,x3,x4,x5,x6,...xn] = [x1,x2,x3...xi] where i = min k, n

-- 1. TESTS 
-- take 3 ['0','1','2','3','4','5','6'] ==> ['0','1','2'] 
-- take 2 [0,1,2,3,4,5,6] ==> [0,1] 
-- take 3 [0,1]           ==> [0,1] 

-- 2. TYPES
mytake :: Int -> [a] -> [a]
mytake _ []    = [] 
mytake 0 _     = [] 
mytake k (h:t) = h : mytake (k-1) t

-- "PURE"
-- String choose3(String str)
-- choose3("caterpillar") 

-- >>> mytake 3 "thisisastring"
-- "thi"

-- mytake 3 "thisisastring" => 't' : 'h' : 'i' : [] 
-- "thi"

-- 3. CODE

-- type String = [Char]

type Text = [Char]

foo :: Text
foo = "this is a silly string"

-- type Date = (Int, Int, Int)
-- type Time = (Int, Int, Int)


data Time = MkTime 
  { hour :: Int
  , minute :: Int
  , second ::  Int
  }

data Date = MkDate 
  { month :: Int
  , day   :: Int 
  , year  :: Int
  }

mkDate :: (Int, Int, Int) -> Date
mkDate (m,d,y) = MkDate m d y

pa2deadlineDate :: Date
pa2deadlineDate = MkDate 1 28 2021

-- >>> :type month
-- month :: Date -> Int

deadlineTime :: Time
deadlineTime = MkTime 23 59 59

extendDate :: Date -> Date 
extendDate = error "TO BE DONE"

newpa2deadline :: Date 
newpa2deadline = extendDate pa2deadlineDate -- deadlineTime

-- (A) YES type error
-- (B) NO  type error


data IntAndString = MkIntString Int String

lastEx :: IntAndString
lastEx = MkIntString 12 "cat"

dateFormat :: Date -> String
dateFormat (MkDate m d y) = monthName m ++ " " ++ show d ++ ", " ++ show y

monthName :: Int -> String
monthName 1 = "January"
monthName 2 = "February"
monthName 3 = "March"
monthName _ = "TBD"

-- >>> dateFormat pa2deadlineDate
-- "January 28, 2021"
-- "30" + 10




