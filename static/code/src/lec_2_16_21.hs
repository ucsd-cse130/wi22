{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveFunctor #-}

module Lec_2_16_21 where

import Prelude hiding (foldl, foldr, map)
import Text.Printf

import Debug.Trace
import Data.Char (toUpper)

shout :: [Char] -> [Char]
-- shout []     = [] 
-- shout (x:xs) = toUpper x : shout xs
shout = map toUpper 

-- >>> squares [1,2,3,4,5,6]
-- [1, 4, 9, 16, 25, 36]

-- squares []     = []
-- squares (x:xs) = (x * x) : squares xs

squares :: [Int] -> [Int]
squares xs = map (\x -> x * x) xs


-- >>> sizes ["this", "is", "the", "end", "my", "friend"]
-- [4,2,3,3,2,6]

map :: (t -> a) -> [t] -> [a]
map f []     = [] 
map f (x:xs) = f x : map f xs


data Table a 
  = Emp 
  | Node String a (Table a) (Table a)
  deriving (Show, Functor)

-- tableMap :: (a -> b) -> Table a -> Table b
-- tableMap f Emp            = Emp
-- tableMap f (Node k v l r) = Node k (f v) (tableMap f l) (tableMap f r) 


menu :: Table Int
menu =  Node "cortado" 4
          (Node "breve" 3
            (Node "americano" 3 Emp Emp)
            (Node "cappucino"  4 Emp Emp)
          )
          (Node "latte" 6
            (Node "espresso" 2 Emp Emp)
            (Node "mocha" 5 Emp Emp)
          )

expensiveMenu :: Table Int
expensiveMenu = fmap (\p -> p * 10)  menu

-- >>> expensiveMenu
-- Node "cortado" 40 (Node "breve" 30 (Node "americano" 30 Emp Emp) (Node "cappucino" 40 Emp Emp)) (Node "latte" 60 (Node "espresso" 20 Emp Emp) (Node "mocha" 50 Emp Emp))




-- >>> size "caterpillar"
-- 11



-- total :: [Int] -> Int 
-- total [] = 0
-- total (x:xs) = x + total xs 

-- cat :: [String] -> String
-- cat []     = ""
-- cat (x:xs) = x ++ cat xs

-- >

total :: [Int] -> Int
total xs = foldr (+) 0 xs

cat :: [String] -> String 
cat xs  = foldr (++) "" xs

foldr op b []     = b
foldr op b (x:xs) = op x (foldr op b xs) 

size' :: [a] -> Int 
size' xs = foldr (\_ v -> 1 + v) 0 xs

-- >>> size' "cat"
-- 3

{- 




foldr op b (x1:(x2:(x3:(x4:[]))))

==> op x1 (foldr op b (x2:(x3:(x4:[]))))

==> op x1 (op x2 (foldr op b (x3:(x4:[])))

==> op x1 (op x2 (op x3 (foldr op b (x4:[]))

==> op x1 (op x2 (op x3 (op x4 (foldr op b []))))

==> op x1 (op x2 (op x3 (op x4 b))))








(\x -> EXPR x) ==== EXPR


-- base = 0, op = +
foo []     = 0
foo (x:xs) = x + foo xs 

-- base = 0, op = \x v -> 1 + v
foo []     = 0
foo (x:xs) = 1 + foo xs

-- base = "", op = ++
foo []     = ""
foo (x:xs) = x ++ foo xs


-}

-- >>> total [10,20,30,40,50]
-- 150

-- >>> cat ("now" : ["i", "am", "hungry"])
-- "nowiamhungry"


sumTR :: [Int] -> Int
sumTR = foldl (+) 0 

catTR :: [String] -> String
catTR = foldl (++) "" 

foldl op acc []    = acc
foldl op acc (h:t) = foldl op (op acc h) t 

-- map/reduce

{- 

foldl op acc (x1 : (x2 : (x3 : (x4 : []))))

==> foldl op (op acc x1) (x2 : (x3 : (x4 : [])))

==> foldl op (op (op acc x1) x2) ((x3 : (x4 : [])))

==> foldl op (op (op (op acc x1) x2) x3) (((x4 : [])))

==> foldl op (op (op (op (op acc x1) x2) x3) x4) ((([])))

-- FOLD-LEFT
==> ((((acc `op` x1) `op` x2) `op` x3) `op` x4) 

-- FOLD-RIGHT
==>  x1 `op` (x2 `op` (x3 `op` (x4 `op` b)))



-}


-- >>> catTR ["10", "20", "30", "40"]
-- "10203040"

{- 

sumTR [10, 20, 30, 40]

==> loop 0        [10, 20, 30, 40]

==> loop 10       [20, 30, 40]

==> loop 30       [30, 40]

==> loop 60       [40]

==> loop 100      []

==> 100


catTR ["10", "20", "30", "40"]

==> loop ""       ["10", "20", "30", "40"]

==> loop "10"     ["20", "30", "40"]

==> loop "1020"   ["30", "40"]

==> loop "102030" ["40"]

==> loop "10203040" []

==> "10203040" 

-}
