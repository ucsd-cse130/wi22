{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Lec_3_3_22 where

import qualified Data.Map as M

import Prelude hiding ( lookup)

data Env k v
  = Def  v              -- default value `v` to be used for "missing" keys
  | Bind k v (Env k v)  -- bind key `k` to the value `v`
  deriving (Show, Functor)

---

emp :: v -> Env k v
emp v = Def v

set :: k -> v -> Env k v -> Env k v
set key val table = Bind key val table

get :: Eq k => k -> Env k v -> v
get key t = case t of
  Def v        -> v
  Bind k v env -> if key == k then v else get key env
---

-- >>> quiz
-- 2.0

quiz :: Double
quiz = read "2"

menu :: Env String Int
menu = set "sandwich" 10 (set "burrito" 11 (set "burger" 15 (emp 0)))

menu_descr :: Env String ([String], Int)
menu_descr = set "sandwich" (["bacon", "lettuce", "tomato"], 10)
              (set "burrito" (["bean", "cheese"], 11)
                (set "burger" (["impossible", "cheese"], 15)
                  (emp ([], 0))
                )
              )


data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Functor)

tree0 :: Tree Int
tree0 = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)

class Mappable t where
  mymap :: (a -> b) -> t a -> t b

instance Mappable [] where
  mymap = map

-- instance Functor Tree where
--   fmap = mymap
instance Mappable Tree where
  mymap f t = case t of
    Leaf       -> Leaf
    Node a l r -> Node (f a) (mymap f l) (mymap f r)

-- instance Functor (Env k) where
--   fmap = mymap

instance Mappable (Env k) where
   mymap = mapEnv

-- >>> fmap fst menu_descr
-- Bind "sandwich" ["bacon","lettuce","tomato"] (Bind "burrito" ["bean","cheese"] (Bind "burger" ["impossible","cheese"] (Def [])))



-- >>> :k Mappable
-- Mappable :: (* -> *) -> Constraint

-- mapList :: (a -> b) ->      [a] ->      [b]
-- mapEnv  :: (a -> b) -> Env k a  -> Env k b



mapEnv :: (t -> v) -> Env k t -> Env k v
mapEnv f env = case env of
  Def x0        -> Def (f x0)
  Bind k v env' -> Bind k (f v) (mapEnv f env')

convert_menu :: Env String ([String], Int) -> Env String Int
convert_menu = mapEnv snd

bump :: Env String Int -> Env String Int
bump = mapEnv (+ 1)

-- { "sandwich" : 10, "burrito" : 11, "burger" : 15, "default" : 0 }

-- >>> jval [menu_descr, menu_descr]
-- JArr [JObj [("sandwich",JObj [("fst",JArr [JStr "bacon",JStr "lettuce",JStr "tomato"]),("snd",JNum 10.0)]),("burrito",JObj [("fst",JArr [JStr "bean",JStr "cheese"]),("snd",JNum 11.0)]),("burger",JObj [("fst",JArr [JStr "impossible",JStr "cheese"]),("snd",JNum 15.0)]),("default",JObj [("fst",JArr []),("snd",JNum 0.0)])],JObj [("sandwich",JObj [("fst",JArr [JStr "bacon",JStr "lettuce",JStr "tomato"]),("snd",JNum 10.0)]),("burrito",JObj [("fst",JArr [JStr "bean",JStr "cheese"]),("snd",JNum 11.0)]),("burger",JObj [("fst",JArr [JStr "impossible",JStr "cheese"]),("snd",JNum 15.0)]),("default",JObj [("fst",JArr []),("snd",JNum 0.0)])]]

class JSON a where
  jval :: a -> JVal

-- >>> jval [1,2,3,4]
-- JArr [JNum 1.0,JNum 2.0,JNum 3.0,JNum 4.0]

instance JSON Double where
  jval n = JNum n

instance JSON Int where
  jval n = JNum (fromIntegral n)
instance JSON Integer where
  jval n = JNum (fromIntegral n)


-- >>> [ x + y | (x, y) <- [(1,2),(3,4),(5,6)] ]
-- [3,7,11]


instance {-# Overlapping #-} JSON String where
  jval s = JStr s

instance JSON a => JSON [a] where
  jval xs = JArr [ jval x | x <- xs ]
                  -- [ jval(x) for x in xs ]
instance (JSON a, JSON b) => JSON (a, b) where
  jval (x, y) = JObj [ ("fst", jval x), ("snd", jval y)]
instance (JSON a, JSON b, JSON c) => JSON (a, b, c) where
  jval (x, y, z) = JObj [ ("fst", jval x), ("snd", jval y), ("thd", jval z)]

instance JSON a => JSON (Env String a) where
  jval env = JObj [ (k, jval v) | (k, v) <- elems env]
        -- = JObj (map (\(k, v) -> (k, jval v)) (elems env))

elems :: Env String a -> [(String, a)]
elems (Def v)         = [("default", v)]
elems (Bind k v rest) = (k, v) : elems rest

-- [ f x | x <- xs ]  is the same as `map f xs`

-- >>> get "coffee" menu
-- 0

data JVal
  = JStr  String
  | JNum  Double
  | JBool Bool
  | JObj  [(String, JVal)]
  | JArr  [JVal]
  deriving (Eq, Ord, Show)

js1 :: JVal
js1 =
  JObj [("name", JStr "Ranjit")
       ,("age",  JNum 41.0)
       ,("likes",   JArr [ JStr "guacamole", JStr "coffee", JStr "bacon"])
       ,("hates",   JArr [ JStr "waiting"  , JStr "grapefruit"])
       ,("lunches", JArr [ JObj [("day",  JStr "monday")
                                ,("loc",  JStr "zanzibar")]
                         , JObj [("day",  JStr "tuesday")
                                ,("loc",  JStr "farmers market")]
                         , JObj [("day",  JStr "wednesday")
                                ,("loc",  JStr "hare krishna")]
                         , JObj [("day",  JStr "thursday")
                                ,("loc",  JStr "faculty club")]
                         , JObj [("day",  JStr "friday")
                                ,("loc",  JStr "coffee cart")]
                         ])
       ]



data Result a = Err String | Ok a

{- Exercise: Write a Functor instance for Result -}

-- instance Functor Result where
--   fmap f r = case r of
--     Err s -> _
--     Ok a -> _
