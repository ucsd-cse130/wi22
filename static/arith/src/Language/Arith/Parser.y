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

-- Operators
-- %left '+' '-'
-- %left '*' '/'
%%

Aexpr : BinExp                   { $1           }
      | TNUM                     { AConst $1    }
      | ID                       { AVar   $1    }
      | '(' Aexpr ')'            { $2           }

BinExp : Aexpr '+' Aexpr         { APlus  $1 $3 } 
       | Aexpr '-' Aexpr         { AMinus $1 $3 }
       | Aexpr '*' Aexpr         { AMul   $1 $3 }
       | Aexpr '/' Aexpr         { ADiv   $1 $3 }
       
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
