---
title: A Tutorial On Parsing with Alex and Happy
date: 2018-05-12
headerImg: books.jpg
---

In this note we consider an invaluable programming tool, the
*parser generator*. The problem that we want to solve
is: how do we **parse strings**, that is, convert
(unstructured) strings, the lowest-level representation of a
program text, into (highly structured) representations like
expressions, statements, functions *etc* which  can then be
compiled or interpreted.

Of course, the problem is much more general and arises in
pretty much every large scale system, how do you convert
raw data strings, into structured objects that can be
manipulated by the rest of the system.

Of course, one can imagine various convoluted algorithms
for extracting structure from strings. Indeed, you may well
think that the conversion routine depends heavily on the
*target* of the conversion! However, it turns out that we
can design a small *domain-specific language* that describes
a large number of the kinds of target structures, and we
will use a *parser generator* that will automatically
convert the structure description into a parsing function!

# An Arithmetic Interpreter

As a running example, let us build a small interpreter for
a language of arithmetic expressions, described by the type

```haskell
data Aexpr 
  = AConst  Int
  | AVar    String
  | APlus   Aexpr Aexpr
  | AMinus  Aexpr Aexpr
  | AMul    Aexpr Aexpr
  | ADiv    Aexpr Aexpr
``` 

shown in file [Types.hs][0]. This expression language
is quite similar to what you saw for the random-art
assignment, and we can write a simple recursive
evaluator for it

```haskell
eval :: Env -> Aexpr -> Value
eval _   (AConst i)     = i
eval env (AVar   x)     = fromMaybe (errUnbound x) (lookup x env)
eval env (APlus  e1 e2) = eval env e1 + eval env e2
eval env (AMinus e1 e2) = eval env e1 - eval env e2
eval env (AMul   e1 e2) = eval env e1 * eval env e2
eval env (ADiv   e1 e2) = eval env e1 `div` eval env e2
```

Here the `env` is a `[(String, Value)]` corresponding to a list
of variables and their corresponding values. Thus, if you run the 
above, you would see something like

```haskell
λ> eval [] (APlus (AConst 2) (AConst 6))
8

λ> eval [("x", 16), ("y", 10)] (AMinus (AVar "x") (AVar "y"))
6

λ> eval [("x", 16), ("y", 10)] (AMinus (AVar "x") (AVar "z"))
*** Exception: Error {errMsg = "Unbound variable z"}
```

Now it is rather tedious to write expressions like
`APlus (AConst 2) (AConst 6)`, and `AMinus (AVar "x") (AVar "z")`.
We would like to obtain a simple parsing function

```haskell
parse :: String -> Aexpr
```

that converts a string to the corresponding `Aexpr` if possible. 
For example, it would be sweet if we could get

```haskell
λ> parse "2 + 6"
APlus (AConst 2) (AConst 6)
λ> parse "(x - y) / 2"
ADiv (AMinus (AVar "x") (AVar "y")) (AConst 2)
```

and so on. Lets see how to get there.

# Strategy

We will use a two-step strategy to convert raw strings into structured
data.

## Step 1 (Lexing) : From String to Tokens

Strings are really just a list of very low-level characters.
In the first step, we will aggregate the characters into more
meaningful *tokens* that contain more high-level information.
For example, we will can aggregate a sequence of numeric characters
into an integer, and a sequence of alphanumerics (starting with a
lower-case alphabet) into say a variable name.

Thus, as a result of the lexing phase, we can convert a list of
individual characters

![Characters](/static/img/info_parser.001a.jpg)

into a list of *tokens*

![Tokens](/static/img/info_parser.001b.jpg)

### Step 2 (Parsing) : From Tokens to Tree

Next, we will use a special description of the structures we
are trying to generate called a *grammar* to convert the list
of tokens into a tree-like representation of our final structure:

```haskell
APlus (AConst 229) (AMul (AConst 98) (AVar "x2"))
```

The actual algorithms for converting from lists of tokens to
trees are very subtle and sophisticated. We will omit a detailed
description and instead just look at how the structures can
themselves be represented by grammars.

Next, we get into the details of our the above strategy, by
describing exactly what the lexer and parser (generators) do
in terms of their input and output.

# Lexers

We will use the tool called `alex` to automatically obtain
a lexer from a high-level description of what the tokens are and
what what sequences of characters should get mapped to tokens.

## Tokens

The file [Lexer.x][2] describes the set of tokens needed
to represent our simple language

```haskell
data Token
  = NUM    AlexPosn Int
  | ID     AlexPosn String
  | PLUS   AlexPosn
  | MINUS  AlexPosn
  | MUL    AlexPosn
  | DIV    AlexPosn
  | LPAREN AlexPosn
  | RPAREN AlexPosn
  | EOF    AlexPosn
```

Note that the first two tokens, `NUM` and `ID` also carry values with
them, respectively `Int` and `String`; the others just have a field
of type `AlexPosn` which, roughly speaking, is the source position
at which that token was found.

## Regular Expressions

Next, we must describe the sequences of characters that get aggregated
into a particular token. This is done using [regular expressions][7]
defined in the file [Lexer.x][2], which has a sequence of rules 
of the form

```haskell
  [\+]                          { \p _ -> PLUS   p }
  [\-]                          { \p _ -> MINUS  p }
  [\*]                          { \p _ -> MUL    p }
  [\/]                          { \p _ -> DIV    p }
  \(                            { \p _ -> LPAREN p }
  \)                            { \p _ -> RPAREN p }
  $alpha [$alpha $digit \_ \']* { \p s -> ID     p s }
  $digit+                       { \p s -> NUM p (read s) }
```

Each rule is of the form: `| <regexp>	{hs-expr}`.
Intuitively, each regular expression `regexp`
describes a sequence of characters, and when
that sequence is _matched_ in the input string,
the corresponding Haskell expression is evaluated
to obtain the _token_ that corresponds to the match.
Let's see some examples,

```haskell
  [\+]                          { \p _ -> PLUS   p }
  [\-]                          { \p _ -> MINUS  p }
  [\*]                          { \p _ -> MUL    p }
  [\/]                          { \p _ -> DIV    p }
  \(                            { \p _ -> LPAREN p }
  \)                            { \p _ -> RPAREN p }
```

- when a character `+`, `-`, `*`, `/` etc. are encountered, 
  the lexer generates the tokens `PLUS`, `MINUS`, `MUL`, `DIV`
  etc. respectively,

- `[c1 c2 ... cn]` where each `ci` is a character denotes
  a regular expression that matches **any of** the characters
  in the sequence. Thus, the regexp `[a-zA-Z]` indicates any
  of the alphabets lower, or upper case and `[0-9]` denotes 
  _any of_ the numeric digits

```haskell
  [0-9]+                           { \p s -> NUM p (read s) }
```

- Thus, `[0 - 9]` denotes a regexp that matches any
  digit-character. When you take a regexp and put a
  `+` in front of it, i.e. `e+` corresponds to
  **one-or-more** repetitions of `e`.
  Thus, the regexp `[0-9]+` matches a _non-empty_ 
  sequence of digit characters!
  In the Haskell expression `p` is the source position, and `s` 
  is the string corresponding to the matching characters; we return 
  the exact `Int` by computing `read s` which converts the matched 
  `String` into an `Int`.

```haskell
  [a-z A-Z] [a-z A-Z 0-9 \_ \']*   { \p s -> ID     p s }
```

- `e1 e2` denotes a regexp that matches any string `s`
  that can be split into two parts `s1` and `s2`
  (s.t. `s == s1 ++ s2`) where `s1` matches `e1` and
  `s2` matches `e2`. That is, `e1 e2` is a **sequencing**
  regexp that first matches `e1` and then matches `e2`.

- `e*` corresponds to **zero-or-more repetitions** of `e`.
  Thus, `[a-zA-Z][a-z A-Z 0-9 \_ \']*` is a regexp that
  matches all strings that (1) begin with an alphabet,
  and then have a (possibly empty)
  sequence of alpha-numeric characters, or underscore or `'`.
  As before, the entire matching string is bound to the
  variable `s` and in this case the `ID p s` token is returned indicating that an identifier appeared in the input stream.

We can tidy up the lexer by **naming** some common regexps nicely,
e.g. writing

```haskell
$digit = 0-9
$alpha = [a-zA-Z]
```

and then simplifying the rules to:

```haskell
  $alpha [$alpha $digit \_ \']* { \p s -> ID     p s }
  $digit+                       { \p s -> NUM p (read s) }
```


## Running the Lexer

We can run the lexer directly to look at the sequences
of tokens found. The function `parseTokens` simply
converts an input string into a buffer on which 
the actual lexer operates.

```haskell
λ> parseTokens "23 + 4 / off -"
Right [ NUM (AlexPn 0 1 1) 23
      , PLUS (AlexPn 3 1 4)
      , NUM (AlexPn 5 1 6) 4
      , DIV (AlexPn 7 1 8)
      , ID (AlexPn 9 1 10) "off"
      , MINUS (AlexPn 13 1 14) 
      ]
```

For each token the above shows the *position* at
which the token was found in the input string.

Note that the the lexer finds *maximal* matches, that is:

```haskell
λ> parseTokens "92zoo"
Right [NUM (AlexPn 0 1 1) 92, ID (AlexPn 2 1 3) "zoo"]
```

Here, when it hits the `z` it knows that the number pattern has ended and
a new variable pattern has begun. Of course, if you give it something that
doesn't match anything, you get an exception

```haskell
λ> parseTokens "%"
Left "lexical error at 1 line, 1 column"
```

# Parsers

Next, will use the tool called `happy` to automatically obtain
a parser from a high-level description of the target structure
called a **grammar**. (Note: grammars are very deep area of study,
we're going to take a very superficial look here, guided by the
pragmatics of how to convert strings to `Aexpr` values.)

## Grammars

A grammar is a recursive definition of a set of trees, comprising

- **Terminals** (aka _Token Names_) which describe the _leaf_ nodes
  of the tree; here the leaf nodes will always be _tokens_ returned 
  by the lexer, so we specify the _terminals_ as:

```haskell
%token
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '/'   { DIV _    }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
```

which says that `TNUM` and `ID` are the "terminals" for the `NUM` and `ID` tokens;
and `'+'`, `'-'` etc are the tokens for the `PLUS`, `MINUS` etc. tokens.

- **Non-terminals** which describe the _internal_ 
  nodes of the tree, respectively, and are written 
  by rules of the form:

```haskell 
NonTerm :
  | Term-or-nonterm-1 ... Term-or-non-term-n { Hs-Expr }
```

  that describe the possible configuration of
  children of each internal node, together with a Haskell
  expression that generates a *value* that is used to
  decorate the node. This value is computed from the
  values decorating the respective children.

For example, the following rules for non-terminals define 
the grammar for `Arith` expressions:

```haskell
Aexpr : BinExp                   { $1           }
      | TNUM                     { AConst $1    }
      | ID                       { AVar   $1    }
      | '(' Aexpr ')'            { $2           }

BinExp : Aexpr '*' Aexpr         { AMul   $1 $3 }
       | Aexpr '+' Aexpr         { APlus  $1 $3 } 
       | Aexpr '-' Aexpr         { AMinus $1 $3 }
       | Aexpr '/' Aexpr         { ADiv   $1 $3 }
```

Note that the above grammar (almost) directly mimics the
recursive type definition of the expressions.  In the above
grammar, the _two_ non-terminals are `Aexpr` and `BinExp`
(we could call them whatever we like, we just picked the
same name for convenience.)

The rules `AExpr` define it to be one of:

- a `BinExp` (which will be expressions made out of a binary-operator), or
- a `TNUM` i.e. a concrete number, or
- a `ID` i.e. a variable, or
- an expression surrounded by parentheses.

The terminals are the tokens we defined earlier, and each
rule corresponds to how you would take the sub-trees (i.e.
sub-expressions) and stitch them together to get bigger trees.

The line

```haskell
%name aexpr
```

at the top tells `happy` to use the rules for
the _non-terminal_ `AExpr` to generate a function
`aexpr` that _parses_ a `Token` stream into an `AExpr`.

Next, let us consider each of the rules in turn.

```haskell
      | TNUM                     { AConst $1    }
      | ID                       { AVar   $1    }
```

- The base-case rules for `TNUM` and `ID` state that those
  (individual) tokens can be viewed as corresponding to `Aexpr`
  nodes. Consider the target expression in the curly braces.
  Here `$1` denotes the value decorating the 1st (and only!)
  element of the corresponding non/terminal- sequence. That is,
  for the former (respectively latter) case `$1` the `Int`
  (respectively `String` value) associated with the token,
  which we use to obtain the base arithmetic expressions via the
  appropriate constructors.

```haskell
       | '(' Aexpr ')'            { $2           }
```

- The last rule allows us to parse parenthesized expressions;
  if there is a left-paren token followed by an expresssion
  followed by a matching right-paren token, then the whole
  sequence is an `Aexpr` node. Notice how the decorated
  expression is simply `$2` which decorates the second
  element of the sequence, i.e. the (sub) expression being
  wrapped in parentheses.

```haskell
BinExp : Aexpr '*' Aexpr         { AMul   $1 $3 }
       | Aexpr '+' Aexpr         { APlus  $1 $3 } 
       | Aexpr '-' Aexpr         { AMinus $1 $3 }
       | Aexpr '/' Aexpr         { ADiv   $1 $3 }
```

- The recursive case rules, e.g. for the `+` case says that
  if there is a token-sequence that is parsed into an `Aexpr`
  node, followed by a `+` token, followed by a sequence that
  is parsed into an `Aexpr` node, then the **entire** sequence
  can be parsed into an `Aexpr` node.
  Here `$1` and `$3` refer to the _first_ and _third_ elements of
  the sequence, that is, the _left_ and _right_ subexpressions.
  The decorated value is simply the super-expression obtained by
  applying the `APlus` constructor to the left and right 
  subexpressions. The same applies to

## Running the Parser

Great, lets take our parser out for a spin! First, lets build the different
elements

```bash
$ cp src/Language/Arith/Parser0.y src/Language/Arith/Parser.y
$ stack build
...
shift/reduce conflicts:  16
...
```

and now we can load up the `ghci` shell with:

```bash
$ stack ghci
```

which puts us in a ghci shell with the parser loaded in:

```haskell
λ> evalString [] "1 + 3 + 6"
10

λ> evalString [("x", 100), ("y", 20)] "x - y"
80
```

And lo! we have a simple calculator that also supports variables.

## Precedence and Associativity

Ok, looks like our calculator works fine, but lets try this

```haskell
λ> evalString [] "2 * 5 + 5"
20
```

Huh?! you would think that the above should yield `15` as `*` has higher
precedence than `+` , and so the above expression is really `(2 * 5) + 5`.
Indeed, if we took the trouble to put the parentheses in, the right thing
happens

```haskell
λ> evalString [] "(2 * 5) + 5"
15
```

Indeed, the same issue arises with a single operator

```haskell
λ> evalString [] "2 - 1 - 1"
2
```

What happens here is that the grammar we gave is **ambiguous**
as there are multiple ways of parsing the string `2 * 5 + 5`, namely

- `APlus (AMul (AConst 2) (AConst 5)) (AConst 5)`, or
- `AMul  (AConst 2) (APlus (AConst 5) (AConst 5))`

We want the former, but `happy` gives us the latter!
Similarly, there are multiple ways of parsing `2 - 1 - 1`, namely

- `AMinus (AMinus (AConst 2) (AConst 1)) (AConst 1)`, or
- `AMinus (AConst 2) (AMinus (AConst 1) (AConst 1))`

Again, since `-` is left-associative, we want the former, but
we get the latter! (Incidentally, this is why we got those wierd
grumbles about `shift/reduce conflicts` when we ran `stack build`
above ...)

There are various ways of adding precedence, one is to hack the
grammar by adding various extra non-terminals, as done here
[Parser2.y][4]. Note how there are no conflicts if you
use that grammar instead.

However, since this is such a common problem, there is a much
simpler solution, which is to add precedence and associativity
annotations to the .mly file. In particular, let us use the
modified grammar [Parser1.y][3].

```haskell 
$ cp src/Language/Arith/Parser1.y src/Language/Arith/Parser.y
$ stack build
```

check it out, no conflicts this time! The only difference between this
grammar and the previous one are the lines

```haskell 
%left '+' '-'
%left '*' '/'
```

This means that all the operators are **left-associative**  
so `e1 - e2 - e3` is parsed as if it were `(e1 - e2) - e3`.
Now, after doing 

```bash 
$ stack ghci 
```

we get

```haskell
λ> evalString [] "2 - 1 - 1"
0
```

Furthermore, we get that addition and subtraction have lower
precedence than multiplication and division (the order of the
annotations matters!)

```haskell
λ> evalString [] "2 * 5 + 5"
15
λ> evalString [] "2 + 5 * 5"
27
```

Hence, the multiplication operator has higher precedence than the addition,
as we have grown to expect, and all is well in the world.

This concludes our brief tutorial, which should suffice for your Nano
programming assignment. However, if you are curious, I encourage you to
look at the documentation for [alex](https://www.haskell.org/alex/) and 
[happy](https://www.haskell.org/happy/) for more details.

[0]: https://github.com/cse130-sp18/arith/blob/master/src/Language/Arith/Types.hs 
[1]: https://github.com/cse130-sp18/arith/blob/master/src/Language/Arith/Parser0.y
[2]: https://github.com/cse130-sp18/arith/blob/master/src/Language/Arith/Lexer.x
[3]: https://github.com/cse130-sp18/arith/blob/master/src/Language/Arith/Parser1.y
[4]: https://github.com/cse130-sp18/arith/blob/master/src/Language/Arith/Parser2.y
[7]: http://en.wikipedia.org/wiki/Regular_expression
