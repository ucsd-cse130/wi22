{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Lec_3_8_22 where

import Prelude hiding (lookup)


{-

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-}


{- Exercise: Write a Functor instance for Result -}

mapResult :: (a -> b) -> Result a -> Result b
mapResult f ra = case ra of
  Err str -> Err str
  Val a   -> Val (f a)

-- >>> mapResult (\n -> n + 10) (Val 0)
-- Val 10


{-

(a) Err "oh no!"
(b) 10
(c) Type error
(d) Pattern match fail


-}

data Expr
  = Num Int
  | Add Expr Expr
  | Div Expr Expr
  | Try Expr Expr
  deriving (Show)

eval_ :: Expr -> Int
eval_ (Num n)     = n
eval_ (Add e1 e2) = eval_ e1 + eval_ e2
eval_ (Div e1 e2) = eval_ e1 `div` eval_ e2
eval_ _ = error "OH NO!!!"


boo = 1 == (3 / 3)

one = 1

eval :: Expr -> Result Int

eval (Num n)     = return n

eval (Add e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)

eval (Div e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      if v2 == 0
                        then Err ("oops, DBZ in: " ++ show e2)
                        else return (v1 `div` v2)

eval (Try e1 e2) = eval e1
                    `handle`
                      (\_ -> eval e2)


-- evalA :: Expr -> Result Int

-- evalA (Num n)     = pure n
-- evalA (Add e1 e2) = (+) <$> eval e1 <*> eval e2
-- evalA ()
-- -- eval (Div e1 e2) = do v1 <- eval e1
-- --                       v2 <- eval e2
-- --                       if v2 == 0
-- --                         then Err ("oops, DBZ in: " ++ show e2)
-- --                         else return (v1 `div` v2)

-- eval (Try e1 e2) = eval e1
--                     `handle`
--                       (\_ -> eval e2)

handle :: Result a -> (String -> Result a) -> Result a
handle res h = case res of
  Val v   -> Val v
  Err str -> h str

data Result a = Err String | Val a                      deriving (Eq, Show)
data List   a = Emp        | Con a (List a)             deriving (Eq, Show)

instance Functor List where
  fmap f Emp        = Emp
  fmap f (Con x xs) = Con (f x) (fmap f xs)

instance Monad List where
  -- return :: a -> m a
  return = returnList

  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=)  = ffList

returnList :: a -> List a
returnList x = Con x Emp

-- >>> mMap (\n -> n * 10) (Con 10 (Con 20 Emp))
-- Con 100 (Con 200 Emp)

mMap :: Monad m => (t -> b) -> m t -> m b
mMap f xs = do
  x <- xs
  return (f x)

-- >>> mFilter (\n -> n > 10) [1,2,3,56,78,100, 2]
-- [56,78,100]

mFilter :: (a -> Bool) -> [a] -> [a]
mFilter f xs = do
  x <- xs
  if f x then return x else []

-- >>> [x1,x2,x3] >>= f     ====> [x1] ++ [ ] ++ [x3]

silly xs ys = do
  x <- xs
  y <- ys
  return (x * y)

{-
def silly(xs, ys):
  for x in xs:
    for y in ys:
      yield (x * y)

-}

-- >>> silly [] []
-- []

-- >>> silly [1,2,3] []
-- []

-- >>> silly [] [10,20,30]
-- []

-- >>> silly [1,2,3] [10, 20, 30]
-- [10,20,30,20,40,60,30,60,90]


-- >>> silly (Err "1") (Err "2")
-- Err "1"

-- >>> silly (Err "1") (Val 10)
-- Err "1"

-- >>> silly (Val 42) (Err "2")
-- Err "2"

-- >>> silly (Val 42) (Val 10)
-- Val 420






foo :: List b -> List b
foo cs = do
  c <- cs
  Con c (Con c Emp)

-- >>> foo (Con 'c' (Con 'a' (Con 't' Emp)))
-- Con 'c' (Con 'c' (Con 'a' (Con 'a' (Con 't' (Con 't' Emp)))))

-- >>> ffList [x1, x2, x3] doStuff ==> doStuff x1 ++ doStuff x2 ++ doStuff x3

ffList :: List a -> (a -> List b) -> List b
ffList l doStuff = case l of
  Emp      -> Emp
  Con x xs -> append thing1 thing2
    where
      thing1 = doStuff x
      thing2 = ffList xs doStuff

append :: List b -> List b -> List b
append Emp        ys = ys
append (Con x xs) ys = Con x (append xs ys)


  -- -- (>>=) :: Result a -> (a -> Result b) -> Result b
  -- (>>=) r doStuff =
  --   case r of
  --     Err s -> Err s
  --     Val v -> doStuff v

{-
    e1 >>= \v1 ->
      e2 >>= \v2 ->
        e3 >>= \v3 ->
          e4

do {v1 <- e1;
    v2 <- e2;
    v3 <- e3;
    e4
    }

-}

{-

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

-}

instance Monad Result where
  -- return :: a -> Result a
  return x = Val x

  -- (>>=) :: Result a -> (a -> Result b) -> Result b
  (>>=) r doStuff =
    case r of
      Err s -> Err s
      Val v -> doStuff v





ex0 :: Expr
ex0 = Add (Num 10) (Num 5)

ex1 :: Expr
ex1 = Div ex0 (Num 3)

ex2 :: Expr
ex2 = Add (Num 5) (Num (-5))

ex3 :: Expr
ex3 = Div ex1 ex2

ex4 :: Expr
ex4 = Try ex3 (Num 59)

-- >>> eval ex0
-- Val 15
-- >>> eval ex1
-- Val 5

-- >>> eval ex2
-- Val 0

-- >>> eval ex3
-- Err "oops, DBZ in: Add (Num 5) (Num (-5))"
--

-- >>> eval ex4
-- Val 59










instance Functor Result where
  fmap = mapResult











{-

map  :: (a -> b) -> [a] -> [b]
map2 :: (a1 -> a2 -> b) -> [a1] -> [a2] -> [b]
map3 :: (a1 -> a2 -> a3 -> b) -> [a1] -> [a2] -> [a3] -> [b]

class Applicative f where
  pure  :: a -> m a
  (<*>) :: f (a -> b) -> f a -> f b

-}

instance Applicative List where

instance Applicative Result where
  pure    = Val
  f <*> x = case (f, x) of
              (Val ff, Val xx) -> Val (ff xx)
              (Err msg, _)     -> Err msg
              (_, Err msg)     -> Err msg

{-
  build [x] = Leaf x

  build [10,20,3,123,1,12,3,1,14,14,4,2,452, 45] = Node x l r
      x = 10
      ls = all elems in xs < x
      rs = all elems in xs > x
      l = build ls
      r = build rs
      -- (ls, rs) = split [20,3,123,1,12,3,1,14,14,4,2,452,45]

  split [20,3,123,1,12,3,1,14,14,4,2,452,45]
      = ([20,3,123,1,12,3], [1,14,14,4,2,452,45])
-}