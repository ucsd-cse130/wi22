{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Test.Tasty
import Common
import Data.List (isInfixOf)
import Language.Arith.Types 
import Language.Arith.Eval 

main :: IO ()
main = runTests [ unit ]

unit :: Score -> TestTree
unit sc = testGroup "Arith"
  [ scoreTest ( parse
              , "123\n\t"
              , AConst 123
              , 1
              , "Number: 123" )
  , scoreTest ( parse
              , "foo"
              , AVar "foo"
              , 1
              , "Var: 'foo'")
  , scoreTest ( parse
              , "x + y"
              , APlus (AVar "x") (AVar "y") 
              , 1
              , "Plus: x + y" )
  , scoreTest ( parse
              , "10 - 2 - 2"
              , AMinus (AMinus (AConst 10) (AConst 2)) (AConst 2)
              , 1
              , "Minus: left-assoc" )
  , scoreTest ( parse
              , "2 + 10 * 3"
              , APlus (AConst 2) (AMul (AConst 10) (AConst 3)) 
              , 1
              , "Mult: binds tighter than plus" )
  ] 
  where 
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

    failTest :: (Show b, Eq b) => (a -> b, a, String, Int, String) -> TestTree
    failTest (f, x, err, n, msg) = scoreTest' sc (expectError err (return . f), x, True, n, msg)

expectError :: (Show b) => String -> (a -> IO b) -> a -> IO Bool
expectError err f x = do { r <- f x; print r; return False }
                      `catch`
                      (return . isInfixOf err . errMsg)

