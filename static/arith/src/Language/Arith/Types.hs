module Language.Arith.Types where

import Control.Exception
import Data.Typeable 
import qualified Data.Maybe as Mb 

data Error = Error {errMsg :: String}
             deriving (Show, Typeable)

instance Exception Error

data Aexpr 
  = AConst  Int
  | AVar    String
  | APlus   Aexpr Aexpr
  | AMinus  Aexpr Aexpr
  | AMul    Aexpr Aexpr
  | ADiv    Aexpr Aexpr
  deriving (Eq, Show)

type Env = [(String, Int)] 

type Value = Int 

eval :: Env -> Aexpr -> Value 
eval _   (AConst i)     = i 
eval env (AVar   x)     = Mb.fromMaybe (errUnbound x) (lookup x env)
eval env (APlus  e1 e2) = eval env e1 + eval env e2
eval env (AMinus e1 e2) = eval env e1 - eval env e2
eval env (AMul   e1 e2) = eval env e1 * eval env e2
eval env (ADiv   e1 e2) = eval env e1 `div` eval env e2

errUnbound :: String -> a 
errUnbound x = throw (Error ("Unbound variable " ++ x))