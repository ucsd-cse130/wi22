module Main where
-- 
import Text.Printf
import qualified Data.List as L

import Data.List
import Data.Maybe

-- Hello, World!

returnList :: a -> [a]
returnList x = [x]

foo :: [Int] -> [Int]
foo xs = do
  x <- xs
  return (x * 10)


-- mMap :: (a -> b) -> [a] -> [b]
mMap :: Monad m => (a -> b) -> m a -> m b
mMap f xs = do
  x <- xs
  return (f x)


-- >>> mMap (* 10) []
-- []

mFilter :: (a -> Bool) -> [a] -> [a]
mFilter p xs = do
  x <- xs
  if p x
    then [x]
    else []

-- >>> mFilter isEven [1,2,3]
-- [2]

{- 
mFilter isEven [1,2,3]

==> 
  [1, 2, 3] >>= \x -> if isEven x then [x] else [] 

==> 
  [doStuff 1] ++ [doStuff 2] ++ [doStuff 3]
==> 
  [] ++ [2] ++ []
==> 
  [2]

-}


isEven n = n `mod` 2 == 0

-- >>> mFilter isEven [0..10]
-- [0,2,4,6,8,10]

{- 

[x1, x2, x3] >>= \x -> 
  [f x]

==> [f x1] ++ [f x2] ++ [f x3]

==> [f x1, f x2, f x3]


foo xs = do
  x <- xs
  return (x * 10)

foo xs = xs >>= (\x -> [x])

foo [1,2,3] 
  ==> [1,2,3] >>= (\x -> [x * 10])
  ==> [10] ++ [20] ++ [30]
  ==> [10,20,30] 

do 
  x1 <- e1
  e2

e1 >>= \x1 -> e2

-- >>> foo [1,2,3]

(a) [1,2,3]
(b) [[1], [2], [3]]
(c) []
(d) ERROR
(e) [3,2,1]

==> 

(a) [10,20,30]
(b) [[10], [20], [30]]
(c) [[10,11,12], [20,21,22], [30,31,32]]
(d) [10,11,12, 20,21,22, 30,31,32]
(e) [12, 22, 32]

-}

bar :: [Int] -> [Int]
bar xs = do
  x <- xs
  y <- [0, 1, 2]
  [x + y]

{- 
bar xs = 
  xs >>= \x ->
    [0,1,2] >>= \y ->
      [x + y]

bar [10,20,30]

==> 
  [10,20,30] >>= \x ->
    [0,1,2] >>= \y ->
      [x + y]

==>
     ([0,1,2] >>= \y ->
      [10 + y])
     ++
     ([0,1,2] >>= \y ->
      [20 + y])
     ++
     ([0,1,2] >>= \y ->
      [30 + y])
==>
     ([10 + 0] ++ [10 + 1] ++ [10 + 2])
     ++
     ([20 + 0] ++ [20 + 1] ++ [20 + 2])
     ++
     ([30 + 0] ++ [30 + 1] ++ [30 + 2])
==> 
  [10,11,12,20,21,22,30,31,32]



[x1, x2, x3] >>= doStuff 

  ==> [doStuff x1] ++ [doStuff x2] ++ [doStuff x3]
 -}

-- >>> bar [10, 20, 30]
-- [10,11,12,20,21,22,30,31,32]


------------------------------------------------

{- 

def main():
  print "hello, world!"

-}

type Recipe a = IO a

silly :: Recipe ()
silly = putStrLn ("Hello!\nWorld!" ++ s)


main :: IO ()
main = sillyApp 

sillyApp :: IO ()
sillyApp = do
  putStrLn "Hey, what's your name?"
  l <- getLine 
  if L.isPrefixOf "exit" l
    then exit 
    else printAndContinue l

exit :: IO ()
exit = putStrLn "Okthxbye!"

printAndContinue :: String -> IO () 
printAndContinue l = do 
  putStrLn ("Hello, " ++ l ++ "!")
  sillyApp 



s :: String
s = printf "(%d %s)" (2 :: Int) ("horse" :: String)

-- >>> s
-- "(2 horse)"

{-

{- D -} combine :: Recipe a  -> Recipe b  -> Recipe b
{- E -} combine :: Recipe a  -> Recipe b  -> Recipe a


crack     :: Recipe Yolk
eggBatter :: Yolk -> Recipe Batter

mkBatter :: Recipe Batter
mkBatter = combineWithResult crack eggBatter

combineWithResult :: Recipe Yolk -> (Yolk -> Recipe Batter) -> Recipe Batter 
                        thing           "doStuffWith"    
What must the type of combineWithResult be?

{- B -} Recipe Yolk -> (Yolk  -> Recipe Batter) -> Recipe Batter
{- D -} Recipe a    -> (a     -> Recipe b     ) -> Recipe b


-} 