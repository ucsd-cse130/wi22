module Lec_2_1_22 where

import Text.Printf
import qualified Text.Printf as Printf

string1 :: [Char]
string1 = "cat"

string2 :: String
string2 = ['c', 'a', 't']

-- >>> [1,2,3] ++ [4,5,6]
-- [1,2,3,4,5,6]

-- >>> string1 ++ "horse" ++ string2 
-- "cathorsecat"

-- >>> dd (MkDate 2 1 2022)
-- 1

data Date = MkDate 
    { mm :: Int 
    , dd :: Int
    , yy :: Int
    }
    deriving (Show)



doc :: [Para]
doc = [ PHeading 1 "I am the title"
      , PText "This is some text" 
      , PList False ["fapple", "coffee"] 
      ]

para1 :: Para
para1 = PHeading 1 "I am the title"
para2 :: Para
para2 = PText "This is some text" 
para3 :: Para
para3 = PList False ["fapple", "coffee"] 


-- >>> paraHtml para2
-- "<h1>I am the title</h1>"

-- >>> paraHtml para2
-- "<p>This is some text</p>"
 
-- >>> paraHtml para3
-- "list case: please fill in"

paraHtml :: Para -> String
paraHtml(PText s) = Printf.printf "<p>%s</p>" s
paraHtml(PHeading n s) = Printf.printf "<h%d>%s</h%d>" n s n
paraHtml(PList b ss) = "list case: please fill in"

data Para
  = PText    String        
  | PHeading Int   String  
  | PList    Bool [String]
  deriving (Show)

bools :: [Bool]
bools = [True, False]

{-

if eCond then eThen else eElse 

case eCond of
  True  -> eThen
  False -> eElse

-}

quiz :: Integer
quiz = case PText "Hey there!" of
        PText    str   -> 1 
        PHeading lvl _ -> 2 
        PList    ord _ -> 3 
{-
p = PText "Hey there!"



-}

-- >>> paraHtml ()

-- (A) squiggles will appear in EDITOR
-- (B) run-time crash
-- (C) just fine but do nothing for Plist

{- 
struct Date {
   int mm;
   int dd;
   int yyyy;
};

deadlineDate.mm

-}

-- >>> deadlineDate 
-- MkDate {mm = 1, dd = 28, yy = 2022}


data Time = MkTime 
                Int         -- hour
                Int         -- min
                Int         -- sec
                   


data Nat 
    = Zero
    | OnePlus Nat
    deriving (Show)

zero :: Nat
zero = Zero
one :: Nat
one  = OnePlus zero
two :: Nat
two = OnePlus one

{- 
toInt two
    ==> toInt (OnePlus one) 
    ==> toInt (OnePlus one) 
    ==> 1 + toInt one
    ==> 1 + toInt (OnePlus Zero)
    ==> 1 + (1 + toInt Zero)
    ==> 1 + (1 + 0)
    ==> 2
-} 


toInt :: Nat -> Int
toInt n = case n of
  Zero -> 0
  OnePlus nat -> 1 + toInt nat

-- toInt Zero        = 0
-- toInt (OnePlus n) = 1 + toInt n 




add :: Nat -> Nat -> Nat
add Zero        m = m
-- add (OnePlus n) m = OnePlus (add n m)
add (OnePlus n) m = add n (OnePlus m)


sub :: Nat -> Nat -> Nat
sub Zero        _           = Zero
sub n           Zero        = n 
sub (OnePlus n) (OnePlus m) = sub n m


----


-- (4.0 + 2.9)  * (3.78 - 5.92)

e0 :: Expr
e0 = EMul 
        (EAdd 
            (ENum 4.0) 
            (ENum 2.9)
        )
        (ESub 
            (ENum 3.78) 
            (ENum 5.92)
        )

calc :: Expr -> Double
calc e = case e of
  ENum x -> x
  EAdd e1 e2 -> calc e1 + calc e2 
  ESub e1 e2 -> calc e1 - calc e2 
  EMul e1 e2 -> calc e1 * calc e2 

-- >>> calc e0 
-- -14.766000000000002


data Expr 
    = ENum Double
    | EAdd Expr Expr 
    | ESub Expr Expr 
    | EMul Expr Expr 
    deriving (Show)
