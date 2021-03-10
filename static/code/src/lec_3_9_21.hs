{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lec_3_9_21 where 
import Prelude hiding (showList)
import Numeric

-----------------------------------------------------------
-- data List a = [] | (:) a (List a)

mapList :: (a -> b) -> [a] -> [b]
mapList f []     =  []
mapList f (x:xs) =  f x : mapList f xs

-- >>> mapList (** 2) [1,2,3,4,5]
-- [1.0,4.0,9.0,16.0,25.0]

-----------------------------------------------------------
-- data Maybe a = Nothing | Just a deriving (Show)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b 
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

-- >>> mapMaybe (** 2) (Just 9)
-- Just 81.0

-----------------------------------------------------------

data Env val 
   = Def  val 
   | Bind String val (Env val) 
   deriving (Show) 

mapEnv :: (a -> b) -> Env a -> Env b
mapEnv f (Def v)         = Def (f v)
mapEnv f (Bind k v rest) = Bind k (f v) (mapEnv f rest)

env0 :: Env Double
env0 = Bind "cat" 10.0 (Bind "dog" 20.0 (Def 0))

-- >>> mapEnv (** 2) env0
-- Bind "cat" 100.0 (Bind "dog" 400.0 (Def 0.0))


-- mapList  :: (a -> b) -> List  a -> List  b
-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b 
-- mapEnv   :: (a -> b) -> Env   a -> Env   b

-----------------------------------------------------------
class Mappable t where
   mymap :: (a -> b) -> t a -> t b

instance Mappable [] where
   mymap = mapList 

instance Mappable Maybe where
   mymap = mapMaybe

instance Mappable Env where
   mymap = mapEnv

-----------------------------------------------------------

-- >>> mymap (** 2) env0
-- Bind "cat" 100.0 (Bind "dog" 400.0 (Def 0.0))


-- example = mymap (** 2) [1,2,3,4,5]

data Expr
  = Number Int
  | Plus   Expr Expr
  | Minus  Expr Expr
  | Mult   Expr Expr
  | Div    Expr Expr
  | Try    Expr Int
  deriving (Show)


data Result a 
   = Error String 
   | Value a
   deriving (Eq, Show, Foldable)


evalBin :: (Int -> Int -> b) -> Expr -> Expr -> Result b
evalBin op e1 e2 = do 
   v1 <- eval e1 
   v2 <- eval e2 
   return (v1 `op` v2) 

-- >>>  10 + 20
-- 30



eval :: Expr -> Result Int

eval (Number n)   = return n

eval (Plus e1 e2)  = evalBin (\x y -> x + y) e1 e2
eval (Minus e1 e2) = evalBin (-) e1 e2 
eval (Mult e1 e2)  = evalBin (*) e1 e2 

eval (Div e1 e2) =  do v1 <- eval e1
                       v2 <- eval e2
                       if v2 == 0 
                         then throw ("Oh no DBZ due to: " ++ show e2)
                         else return (v1 `div` v2)

eval (Try e n)   = eval e `catch` (\_ -> Value n)
   
catch :: Result a -> (String -> Result a) -> Result a
catch (Error msg) handler = handler msg
catch (Value v)   _       = Value v

throw :: String -> Result a
throw = Error

bind :: Result a -> (a -> Result b) -> Result b
bind e doStuffWith =
  case e of
    Error msg -> Error msg
    Value v   -> doStuffWith v

ret :: a -> Result a
ret x = Value x

instance Monad Result where
  (>>=)  = bind
  return = ret 



{- 

class Eq a where
   (==) :: a -> a -> Bool

class Ord a where
   (<=) :: a -> a -> Bool


class Monad t where
  (>>=)  :: t a -> (a -> t b) -> t b
  return :: a -> t a

- Int -> Result Int
- Int -> [Int]

-}

-- >>> foldr (+) 0 (Error "oops")
-- 0

data List a = Nil | Cons a (List a) deriving (Show, Functor)

instance Monad List where
  -- return  :: a -> List a
  -- (>>=)   :: List a -> (a -> List b) -> List b

returnList :: a -> [a]
returnList x = [x]

bindList :: [a] -> (a -> [b]) -> [b]
bindList []     f = []
bindList (x:xs) f = f x ++ bindList xs f 

foo xs ys = do
  x <- xs
  y <- ys
  return (x, y)

{- 
for x in xs:
  for y in ys:
    yield (x, y)
 -}

--  :: List (Int, String)

-- >>> foo ["1", "2", "red", "blue"] ["fish"]
-- [("1","fish"),("2","fish"),("red","fish"),("blue","fish")]


-- >>> foo [] ["cat", "dog"]
-- []

-- data Result a = Error String | Value a
-- data List   a = []           | (:)   a (List a)






-- (>>=)
-- 


-- >>> eval (Div (Number 6) (Number 3)) 
-- Value 2

-- >>> eval (Div (Number 6) (Plus (Number 3) (Number (-3))))
-- Error "Oh no DBZ due to: Plus (Number 3) (Number (-3))"

-- >>> eval (Try (Div (Number 6) (Plus (Number 3) (Number (-3)))) 9)
-- Value 9

-- Num a
-- +,- 



myDiv x 0 = Nothing
myDiv x y = Just (x / y)


instance Applicative List where


instance Applicative Result where

instance Functor Result where
   fmap = mapResult 

mapResult :: (a -> b) -> Result a -> Result b
mapResult f (Error msg) = Error msg 
mapResult f (Value v)   = Value (f v)


intDiv :: Int -> Int -> Double
intDiv x y = x' / y' 
  where 
     x'    = fromIntegral x
     y'    = fromIntegral y

