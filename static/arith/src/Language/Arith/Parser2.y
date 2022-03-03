{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Arith.Parser (
    parseAexpr
  , parseTokens
  ) where

import Language.Arith.Lexer
import Language.Arith.Types hiding (Arith (..))
import Control.Monad.Except
import Control.Exception
}

-- Entry point
%name aexpr

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '/'   { DIV _    }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }

%%

-- SUM-OF-PRODUCT

--  (2 * 5 * 3) + (10 * 7)

-- ((((((10) + (20)) + (30 * 49 * 41)))) - (29 * 80))

-- Aexpr -> (((10 + 20) + ((30 * 49) * 41) - (29 * 80) )

Aexpr : Aexpr '+' Aexpr2         { APlus $1 $3  }
      | Aexpr '-' Aexpr2         { AMinus $1 $3 }
      | Aexpr2                   { $1 }

Aexpr2 : Aexpr2 '*' Aexpr3       { AMul $1 $3 }
       | Aexpr2 '/' Aexpr3       { ADiv $1 $3 }
       | Aexpr3                  { $1 }

Aexpr3 : TNUM                    { AConst $1 }
       | ID                      { AVar   $1 }
       | '(' Aexpr ')'           { $2 }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"


parseAexpr :: String -> Aexpr
parseAexpr s = case parseAexpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseAexpr' input = runExcept $ do
   tokenStream <- scanTokens input
   aexpr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}
