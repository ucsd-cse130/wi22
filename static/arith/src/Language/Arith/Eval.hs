{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Language.Arith.Eval
  ( 
  -- * Run a file, string or expr 
    evalFile
  , evalString
  , evalExpr

  -- * Convert string to AST
  , parse
  -- * Convert string to Tokens
  , tokens)
  where

import Control.Exception (catch)
import Language.Arith.Types
import Language.Arith.Lexer ( Token ) 
import Language.Arith.Parser

--------------------------------------------------------------------------------
evalFile :: FilePath -> IO () 
--------------------------------------------------------------------------------
evalFile f = runFile f `catch` exitError

runFile :: FilePath -> IO ()
runFile f = do 
  str  <- readFile f 
  let n = evalString [] str
  putStrLn ("Result = " ++ show n) 

exitError :: Error -> IO () 
exitError (Error msg) = putStrLn ("Error: " ++ msg) 

--------------------------------------------------------------------------------
evalString :: Env -> String -> Value 
--------------------------------------------------------------------------------
evalString env s = evalExpr env (parseAexpr s)

--------------------------------------------------------------------------------
evalExpr :: Env -> Aexpr -> Value 
--------------------------------------------------------------------------------
evalExpr = eval 

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "123"
-- AConst 123
--
-- >>> parse "foo"
-- AVar "foo"
--
-- >>> parse "x + y"
-- APlus (AVar "x") (AVar "y")
--
-- >>> parse "10 - 2 - 2"
-- AMinus (AMinus (AConst 10) (AConst 2)) (AConst 2)
--
-- >>> parse "2 + 10 * 3"
-- APlus (AConst 2) (AMul (AConst 10) (AConst 3)) 

--------------------------------------------------------------------------------
parse :: String -> Aexpr
--------------------------------------------------------------------------------
parse = parseAexpr

--------------------------------------------------------------------------------
tokens :: String -> Either String [Token]
--------------------------------------------------------------------------------
tokens = parseTokens