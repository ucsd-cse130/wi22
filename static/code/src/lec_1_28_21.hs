{-# LANGUAGE PartialTypeSignatures #-}

module Lec_1_28_21 where

import Text.Printf


data IntAndChar = MkIntChar { icInt :: Int, icChar :: Char }

ic :: IntAndChar
ic = MkIntChar 12 'c'


-- >>> icInt ic       
-- 12

-- >>> icChar ic     
-- 'c'


data Paragraph 
  = PText    String         
  | PHeading Int String 
  | PList    Bool [String]

doc :: [Paragraph]
doc = [ PHeading 1 "Notes from 130"
      , PText "There are two types of languages:" 
      , PList False [ "those people complain about" 
                   , "those no one uses"]
      ]

-- >>> (mkHtml doc)
-- /Users/rjhala/teaching/130-wi21/static/code/src/lec_1_28_21.hs:(42,1)-(46,71): Non-exhaustive patterns in function paraHtml

-- ["<h1>Notes from 130</h1>","<p>There are two types of languages:</p>","<ol> <li> those people complain about </li>","<li> those no one uses </li>"," </ol>"]

mkHtml :: [Paragraph] -> String
mkHtml []     = ""
mkHtml (p:ps) = paraHtml p ++ "\n" ++ mkHtml ps

paraHtml :: Paragraph -> String
paraHtml (PText text       ) = printf "<p>%s</p>" text
paraHtml (PHeading lvl text) = printf "<h%d>%s</h%d>" lvl text lvl 
paraHtml (PList True elems ) = printf "<ol> %s </ol>" (elemsHtml elems)
paraHtml (PList False elems) = printf "<ul> %s </ul>" (elemsHtml elems) 


elemsHtml :: [String] -> String 
elemsHtml [] = ""
elemsHtml (e:es) = printf "<li> %s </li>\n" e ++ elemsHtml es   


box1 :: Paragraph
box1 = PText "Hey there!"

p1 :: Paragraph
p1 = PHeading 1 "Notes from 130"

p2 :: Paragraph
p2 = PText "There are two types of languages:" 

p3 :: Paragraph
p3 = PList True [ "those people complain about", "those no one uses"]


data Day  
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Show)

dayInt :: Day -> Int
dayInt Mon = 1
dayInt Tue = 2 
dayInt Wed = 3 
dayInt Thu = 4 
dayInt Fri = 5 
dayInt Sat = 6 
dayInt Sun = 7 

instance Num Day where
  d1 + d2 = intDay (dayInt d1 + dayInt d2)
  fromInteger = intDay

intDay :: (Eq t, Num t) => t -> Day
intDay 1 = Mon 
intDay 2 = Tue  
intDay 3 = Wed  
intDay 4 = Thu  
intDay 5 = Fri  
intDay 6 = Sat  
intDay 0 = Sun  
intDay n = intDay (n - 7)

{-

 data Bool 
   = False 
   | True

  if cond then e1 else e2 

  case cond of
    True  -> e1
    False -> e2


 -}

data K = KInt Int | KStr String | KBoo Bool

newQuiz :: K
newQuiz = 
  let p = PText "Hey there!"
  in case p of
      PHeading lvl _ -> KInt lvl
      PText    str   -> KStr str 
      PList    ord _ -> KBoo ord 



-- sumList :: [Int] -> Int
sumList :: Num p => [p] -> p
sumList [] = 0
sumList (x:xs) = x + sumList xs



-- >>> sumList [Mon,Tue]
-- Wed


