module Lec_1_27_22 where

import Text.Printf

string1 :: [Char]
string1 = "cat"

string2 :: String
string2 = ['c', 'a', 't']

-- >>> [1,2,3] ++ [4,5,6]
-- [1,2,3,4,5,6]

-- >>> string1 ++ "horse" ++ string2 
-- "cathorsecat"

data Date = MkDate 
    { mm :: Int 
    , dd :: Int
    , yy :: Int
    }
    deriving (Show)

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
                   

getHour :: Time -> Int 
getHour t = case t of 
    MkTime h _ _ -> h



deadlineDate :: Date 
deadlineDate = MkDate 1 28 2022

deadlineTime :: Time 
deadlineTime = MkTime 11 59 59

-- foo :: Date
-- foo = extendDateByOne deadlineTime

-- (mon, day, year)

extendDateByOne :: Date -> Date 
extendDateByOne = undefined

-- monthDays :: [(Int, Int)]
-- monthDays = [(1, 31), (2, 28), (3, 31)]

-- >>> putStrLn (docHtml doc)

doc :: [Para]
doc = [ MkHeading 1 "Notes from 130"                        -- Level 1 heading
      , MkPlain     "There are two types of languages:"     -- Plain text
      , MkList    True  [ "those people complain about"     -- Ordered list
                        , "those no one uses"]
      ]

docHtml ::  [Para] -> String
docHtml ps = case ps of
  [] -> ""
  pa : pas -> paraHtml pa ++ "\n" ++ docHtml pas

paraHtml :: Para -> String
paraHtml p = case p of 
  MkHeading n s -> headingHtml n s
  MkPlain s -> "<p>" ++ s ++ "</p>"
  MkList ord items -> listHtml ord items 

listHtml :: Bool -> [String] -> String
listHtml isOrd strs = printf "<%s>%s</%s>" listTag contents listTag
  where 
      listTag  = if isOrd then "ol" else "ul"
      contents = concat [ "<li>" ++ str ++ "</li>" | str <- strs ] 
                    --  [ "<li>" ++ str ++ "</li>" for str in strs ] 

myList :: [Integer]
myList = [1..10]

-- >>> ["<li>" ++ str ++ "</li>" | str <- ["cat", "dog"] ]
-- ["<li>cat</li>","<li>dog</li>"]


-- >>> myList
-- [1,2,3,4,5,6,7,8,9,10]

headingHtml :: Int -> String -> String
headingHtml level str = "<" ++ hdr ++ ">" ++ str ++ "</" ++ hdr ++ ">"
  where 
      hdr = levelLabel level 

levelLabel :: Int -> String
levelLabel i = "h" ++ show i
{- 

<h1>Notes from 130</h1>
<p>"There are two types of languages:"</p>
<ol>
<li>"those people complain about"</li>
<li>"those no one uses"</li>
</ol>

-}

-- >>> doc
-- [MkHeading 1 "Notes from 130",MkPlain "There are two types of languages:",MkList True ["those people complain about","those no one uses"]]



data Para 
    = PHead  Heading
    | PPlain Plain
    | PList  ParaList
    deriving (Show)

data Heading = MkHeading Int String     deriving(Show)
data Plain   = MkPlain String           deriving(Show)
data ParaList = MkList Bool [String]    deriving(Show)

{- 

List<Paragraph> doc

string paraHtml(para p) {
    if (p instanceof Heading) { 
        ...
    } else if (p instsance)
    instanceOf(p) { 
        ...
    }
}

class Paragraph {
} 

class Heading extends Para { 
    int level;
    string text;
}
class PlainText extends Para { 
    string text;
}
class ParaList extends Para { 
    bool isOrdered;
    List<String> items;
}




-}
