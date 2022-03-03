{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Lec_3_1_22 where

import qualified Data.Map as M

import Prelude hiding ( lookup)

inc :: Num a => a -> a
inc x = x + 1

(===) :: p1 -> p2 -> Bool
(===) x y = False


instance Num Bool where
    True + _    = True
    _    + True = True
    _    + _    = False

-- >>> True * True
-- /Users/rjhala/teaching/130-wi22/static/code/src/lec_3_1_22.hs:17:10-17: No instance nor default method for class operation *


bar :: Eq a => a -> a -> Bool
bar x y = x == y


data Jhala = This | That deriving (Eq, Ord)


-- instance Eq Jhala where
--   This == This = True
--   That == That = True
--   _    == _    = False

instance Show Jhala where
    show This = "jhala::this"
    show That = "jhala::that"

-- >>> This == That
-- False



-- >>> False + False
-- False


data Env k v
  = Def  v              -- default value `v` to be used for "missing" keys
  | Bind k v (Env k v)  -- bind key `k` to the value `v`
  deriving (Show)

{-

data Tree k v
  = Leaf
  | Node k v (Tree k v) (Tree k v)
  deriving (Show)


                   k -> v

            L                  R

-}


v1 :: Int
v1 = 45

v2 :: Double
v2 = 4.5

-- >>> v1 + v2
-- Couldn't match expected type ‘Int’ with actual type ‘Double’

{-

 (+)
 (<=)
 (==)
 show
-}

foo = (+)


exEnv :: Env String Int
exEnv = Bind "dog" 20 (Bind "cat" 10 (Def 0))

get :: Eq k => k -> Env k v -> v
get key env = case env of
    Def v -> v
    Bind k v env' -> if key == k then v else get key env'
