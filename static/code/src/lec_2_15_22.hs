module Lec_2_8_22 where

import Text.Printf
import qualified Text.Printf as Printf
import Data.Char (toUpper)
import Control.Concurrent (yield)

import Prelude hiding (foldr)

-- >>> foo
-- [2,3,4]

addAdjacent :: [Int] -> Int
addAdjacent xs = undefined

{- 
  [1,2,3,4,5]
  [0+1, 1+2, 2+3, 3+4, 4+5 ]

-}

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

myLen :: [a] -> Int
myLen = foldr (\_ r -> 1 + r) 0 

-- sumList [] = 0
-- sumList (x:xs) = x + sumList xs

sumTR :: [Int] -> Int
sumTR = loop 0 
  where
    loop res []     = res 
    loop res (x:xs) = loop (res + x) xs 

-- lenTR = loop 0  
--   where
--     loop res []     = res
--     loop res (x:xs) = loop (res + 1) xs 

lenTR = loop (\res x -> res + 1) 0 

sumTR' = loop (+) 0

concatTR = loop (++) ""

-- concatTR = loop ""
--   where
--     loop res []     = res
--     loop res (x:xs) = loop (res ++ x) xs 


loop op res []     = res
loop op res (x:xs) = loop op (op res x) xs  

append = (++)

plus = (+)

concatList = foldr append        ""
sumList    = foldr plus          0
longest    = foldr (max. length) 0 

-- (\x -> e x) == e
-- >>> sumList [1,2,3,4,5]
-- 15

concatList :: [String] -> String

-- >>> concatList ["this", "that", "cat", "in", "hat"]
-- "thisthatcatinhat"

-- concatList []     = ""
-- concatList (x:xs) = x ++ concatList xs

longest :: [String] -> Int
-- longest []     = 0
-- longest (x:xs) = max (length x) (longest xs)


inc x = x + 1 

bar y = inc (inc (inc y))

bar' = inc . inc . inc 
-- >>> bar' 10
-- 13

-- foo x = gobble (gibble (baz (bar x)))
-- foo = gobble . gibble . baz . bar


longer :: String -> String -> String
longer w1 w2 = if length w1 > length w2 then w1 else w2

foldr:: (a -> b -> b) -> b -> [a] -> b
foldr op b []     = b
foldr op b (x:xs) = op x       (foldr op b xs)

-- >>> myLen [1,2,3,4] 
-- 4



{- 



-}

quiz = foldr (:) [] [1,2,3,4]

-- >>> quiz
-- [1,2,3,4]

{- 

foldr op b (x1 : x2 : x3 : x4 : x5 : [])

=> op x1 (foldr op b (x2:x3:x4:x5:[]))
=> op x1 (op x2 (foldr op b (x3:x4:x5:[])))
=> op x1 (op x2 (op x3 (foldr op b (x4:x5:[]))))
=> op x1 (op x2 (op x3 (op x4 (foldr op b (x5:[])))))
=> op x1 (op x2 (op x3 (op x4 (op x5 (foldr op b [])))))

=> x1 `op` (x2 `op` (x3 `op` (x4 `op` (x5 `op` b))))

-}


-- >>> concatList ["cat", "dog", "horse"] 

-- >>> longest ["cat", "dog", "horse"]
-- 5

-- "horse"
-- "catdoghorse"

-- ==> "cat_dog_horse"

{- 
foo []     = 0
foo (x:xs) = 1          + foo xs

foo []     = 0
foo (x:xs) = x          + foo xs

foo []     = 0
foo (x:xs) = (length x) `max` (foo xs)


foo []     = ""
foo (x:xs) = x          ++ foo xs


concatList = foo (++) ""

-}

plus :: Num a => a -> a -> a
plus x y = x + y

-- >>>  1 `plus` 2
-- 3



rev = foldl (\res x -> x : res) []

buiz = rev [1,2,3,4]

--- >>> buiz
-- [4,3,2,1]

{-

loop [] [1,2,3,4]
=> loop [1] [2,3,4]
=> loop [2,1] [3,4]
=> loop [3, 2,1] [4]
=> loop [4, 3, 2, 1] []
=> [4, 3, 2, 1]

loop op res [x1,x2,x3]
==> loop (op res x1) [x2, x3]
==> loop (op (op res x1) x2) [x3]
==> loop (op (op (op res x1) x2) x3) []
==> (op (op (op res x1) x2) x3)

-}

adda' (h:t) = h : adda (h:t)

adda (x1:x2:rest) = (x1 + x2) : adda (x2:rest)  
adda _            = []

-- >>> adda' [1,2,3]
-- [1,3,5]

{- 

addAdj (1:2:3:[])
  => 3 : addAdj (2:3:[])
  => 3 : 5 : addAdj (3:[])
  => 3 : 5 : []



[1,3,5]

addAdj cur []  = [] 
addAdj cur [x] = (cur + x) : addAdj x xs



-}
