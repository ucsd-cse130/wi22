module Lec_2_8_22 where

import Text.Printf
import qualified Text.Printf as Printf
import Data.Char (toUpper)


incr :: Int -> Int
incr x = x + 1

silly :: Int -> Int
silly = incr

{- 
\a -> f a 
-}
data List a = Nil | Cons a (List a) deriving (Show)

exList :: List Int
exList = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

addList :: List Int -> Int
addList Nil        = 0
addList (Cons h t) = h + addList t 

addListTR :: List Int -> Int
addListTR l = loop 0 l 
  where
    loop :: Int -> List Int -> Int
    loop res Nil        = res  
    loop res (Cons h t) = loop (res + h) t

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

op :: [a] -> a -> [a]
op res x = x : res

revTR :: [a] -> [a]
revTR = loop []
  where
      loop res []     = res
      loop res (x:xs) = loop (x:res) xs
-- >>> revTR [1,2,3,4]
-- [4,3,2,1]

{- 
loop [] [1,2,3,4]
    => loop (1:[]) [2,3,4] 
    => loop (2:(1:[])) [3,4] 
    => loop (3:(2:(1:[]))) [4] 
    => loop (4:(3:(2:(1:[])))) [] 
    => (4:(3:(2:(1:[]))))
    => [4,3,2,1]


def rev(l):
  res 

-}

-- >>> rev [1,2,3,4]
-- [4,3,2,1]


-- >>> addListTR exList
-- 10

-- >>> evens [] 
-- []
-- >>> evens [1,2,3,4] 
-- [2, 4]

evens :: [Int] -> [Int]
evens []     = [] 
evens (x:xs) = if even x 
                then x : evens xs
                else     evens xs 

shorties :: [String] -> [String]
shorties []     = []
shorties (x:xs) = if short x 
                    then x : shorties xs 
                    else     shorties xs 

myFilter :: (s -> Bool) -> [s] -> [s]
myFilter cond []     = []
myFilter cond (x:xs) 
  | cond x      = x : myFilter cond xs 
  | otherwise   =     myFilter cond xs 

shorties' :: [String] -> [String]
shorties' = myFilter short 

evens' :: [Int] -> [Int]
evens'    = myFilter even

-- >>> evens' [1,2,3,4,45]
-- [2,4]

{- 
bob []     = [] 
bob (x:xs) = if even x 
                then x : bob xs
                else     bob xs 

bob []     = []
bob (x:xs) = if short x 
                then x : bob xs 
                else     bob xs 



10 * (29 + 30)  == pat 10 29 30
56 * (91 + 4)   == pat 56 91 4

pat x y z = x * (y + z)


-}




-- >>> shorties ["here", "is", "a", "list", "of", "words"]
-- ["is","a","of"]



short :: String -> Bool
short x = length x <= 2

-- mod x 2 is True if x is "even"



-- >>> veryMad :: ["i", "am", "very", "hungry"]
-- ["I", "AM", "VERY", "HUNGRY"]

veryMad :: [String] -> [String]
veryMad []     = []
veryMad (x:xs) = strUpper x : veryMad xs

strUpper :: [Char] -> [Char] 
strUpper []     = []
strUpper (c:cs) = toUpper c : strUpper cs

veryMad' :: [String] -> [String]
veryMad'  = map strUpper

strUpper' :: [Char] -> [Char]
strUpper' = map toUpper

squares' :: [Int] -> [Int]
squares'  = map (\x -> x*x)


-- PORT OF Midterm SP 15 Q1

data Option a = None | Some a deriving (Show)

find :: (a -> Bool) -> [a] -> Option a
find f xs = case xs of
    []      -> None
    x:xs'   -> if f x 
                then Some x 
                else find f xs'


xs0 :: [Integer]
xs0 = [2,4,8,16,32]

ansD :: Option Integer
ansD = let f x = x > 10 
       in 
         find f xs0

ansE :: Option Integer
ansE = let f x = x `mod` 2 == 1 
       in 
         find f xs0

-- PORT OF Midterm SP 15 Q2

data Set a = MkSet [a] deriving (Show)

empty :: Set a
empty = MkSet []

member :: Int -> Set Int -> Bool
member x s = case s of
    MkSet []    -> False 
    MkSet (h:t) -> h == x || member x (MkSet t)

add :: Int -> Set Int -> Set Int 
add x s = case s of { MkSet as -> MkSet (x:as) }

del :: Int -> Set Int -> Set Int
del x s = case s of { MkSet ns -> MkSet (myFilter neq ns) } 
  where
    neq y = x /= y

-- >>> let s0 = empty
-- >>> let s1 = add 1 s0
-- >>> let s2 = add 2 s1
-- >>> let s3 = del 1 s2
-- >>> (s0, s1, s2, s3)
-- (MkSet [],MkSet [1],MkSet [2,1],MkSet [2])

-- (false, true)


-- >>> ansE
-- None



-- >>> ans
-- Some 16

{- 

type ’a option = None | Some of ’a


(d) 





let ans =
  let x =0 in 
 let a1 = let x = 1 in
fun y z -> [x;y;z]
in
let a2 = let x = 100 in
a1 x
in a2 x


>>> squares [1,2,3,4,5]
[1,4,9,16,25]


foo []     = []
foo (x:xs) = strUpper x : foo xs

foo []     = []
foo (x:xs) = toUpper x : foo xs
-}

foo :: (a -> b) -> [a] -> [b]
foo conv []     = []
foo conv (x:xs) = conv x : foo conv xs


-- >>> stringify [1,2,3]
-- ["1","2","3"]

stringify :: [Int] -> [String]
stringify = map show


{- 

let ISEMPTY = \xs -> xs (\x y z -> FALSE) TRUE

l0 = FALSE

l1 = PAIR ONE l0

ISEMPTY l0 

=*> ISEMPTY FALSE 

=*> FALSE (\x y z -> FALSE) TRUE 

=*> TRUE

ISEMPTY l1

=*> ISEMPTY (PAIR ONE l0)

=*> (PAIR ONE l0) (\x y -> FALSE) TRUE 

=*> (\b -> b ONE l0) (\x y z -> FALSE) TRUE

=*> ((\x y z -> FALSE) ONE l0) TRUE

=*> FALSE

let PAIR x y = \b -> b x y

-}