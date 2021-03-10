---
title: Monads 
date: 2019-06-5
headerImg: books.jpg
---

## Abstracting Code Patterns

a.k.a. **Don't Repeat Yourself**

### Lists

```haskell
data List a
  = []
  | (:) a (List a)
```

<br>
<br>
<br>

### Rendering the Values of a List

```haskell
-- >>> incList [1, 2, 3]
-- ["1", "2", "3"]

showList        :: [Int] -> [String]
showList []     =  []
showList (n:ns) =  show n : showList ns
```

<br>
<br>
<br>

### Squaring the values of a list

```haskell
-- >>> sqrList [1, 2, 3]
-- 1, 4, 9

sqrList        :: [Int] -> [Int]
sqrList []     =  []
sqrList (n:ns) =  n^2 : sqrList ns
```

<br>
<br>
<br>

### Common Pattern: `map` over a list

Refactor iteration into `mapList`

```haskell
mapList :: (a -> b) -> [a] -> [b]
mapList f []     = []
mapList f (x:xs) = f x : mapList f xs
```

Reuse `map` to implement `inc` and `sqr`

```haskell
showList xs = map (\n -> show n) xs

sqrList  xs = map (\n -> n ^ 2)  xs
```

### Trees

```haskell
data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
```

<br>
<br>
<br>

### Incrementing and Squaring the Values of a Tree

```haskell
-- >>> showTree (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
-- (Node "2" (Node "1" Leaf Leaf) (Node "3" Leaf Leaf))

showTree :: Tree Int -> Tree String
showTree Leaf         = ???
showTree (Node v l r) = ???

-- >>> sqrTree (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
-- (Node 4 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf))

sqrTree :: Tree Int -> Tree Int
sqrTree Leaf         = ???
sqrTree (Node v l r) = ???
```

### QUIZ: `map` over a Tree

Refactor iteration into `mapTree`! What should the type of `mapTree` be?

```haskell
mapTree :: ???

showTree t = mapTree (\n -> show n) t
sqrTree  t = mapTree (\n -> n ^ 2)  t

{- A -} (Int -> Int)    -> Tree Int -> Tree Int
{- B -} (Int -> String) -> Tree Int -> Tree String
{- C -} (Int -> a)      -> Tree Int -> Tree a
{- D -} (a -> a)        -> Tree a   -> Tree a
{- E -} (a -> b)        -> Tree a   -> Tree b
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Lets write `mapTree`

```haskell
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf         = ???
mapTree f (Node v l r) = ???
```

### QUIZ

Wait ... there is a common pattern across two _datatypes_

```haskell
mapList :: (a -> b) -> List a -> List b    -- List
mapTree :: (a -> b) -> Tree a -> Tree b    -- Tree
```

Lets make a `class` for it!

```haskell
class Mappable t where
  map :: ???
```

What type should we give to `map`?

```haskell
{- A -} (b -> a) -> t b    -> t a
{- B -} (a -> a) -> t a    -> t a
{- C -} (a -> b) -> [a]    -> [b]
{- D -} (a -> b) -> t a    -> t b
{- E -} (a -> b) -> Tree a -> Tree b
```

<br>
<br>
<br>
<br>
<br>
<br>

### Reuse Iteration Across Types

Haskell's libraries use the name `Functor` instead of `Mappable` 

```haskell
instance Functor [] where
  fmap = mapList

instance Functor Tree where
  fmap = mapTree
```

And now we can do

```haskell
-- >>> fmap (\n -> n + 1) (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
-- (Node 4 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf))

-- >>> fmap show [1,2,3]
-- ["1", "2", "3"]
```

### Exercise: Write a `Functor` instance for `Result`

```haskell
data Result  a
  = Error String
  | Ok    a

instance Functor Result where
  fmap f (Error msg) = ???
  fmap f (Ok val)    = ???
```

When you're done you should see

```haskell
-- >>> fmap (\n -> n ^ 2) (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
-- (Node 4 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf))

-- >>> fmap (\n -> n ^ 2) (Error "oh no") 
-- Error "oh no"

-- >>> fmap (\n -> n ^ 2) (Ok 9) 
-- Ok 81
```

## Next: A Class for Sequencing

Recall our old `Expr` datatype

```haskell
data Expr
  = Number Int
  | Plus   Expr Expr
  | Div    Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Number n)   = n
eval (Plus e1 e2) = eval e1   +   eval e2
eval (Div  e1 e2) = eval e1 `div` eval e2

-- >>> eval (Div (Number 6) (Number 2))
-- 3
```

### But, what is the result

```haskell
-- >>> eval (Div (Number 6) (Number 0))
-- *** Exception: divide by zero
```

A crash! Lets look at an alternative approach to avoid dividing by zero.

The idea is to return a `Result Int`  (instead of a plain `Int`)

- If a _sub-expression_ had a divide by zero, return `Error "..."`
- If all sub-expressions were safe, then return the actual `Result v`

```haskell
eval :: Expr -> Result Int
eval (Number n)   = Value n
eval (Plus e1 e2) = case e1 of
                      Error err1 -> Error err1
                      Value v1   -> case e2 of
                                      Error err2 -> Error err2
                                      Value v1   -> Result (v1 + v2)
eval (Div e1 e2)  = case e1 of
                      Error err1 -> Error err1
                      Value v1   -> case e2 of
                                      Error err2 -> Error err2
                                      Value v1   -> if v2 == 0 
                                                      then Error ("yikes dbz:" ++ show e2)
                                                      else Value (v1 `div` v2)
```

The **good news**, no nasty exceptions, just a plain `Error` result

```haskell
λ> eval (Div (Number 6) (Number 2))
Value 3
λ> eval (Div (Number 6) (Number 0))
Error "yikes dbz:Number 0"
λ> eval (Div (Number 6) (Plus (Number 2) (Number (-2))))
Error "yikes dbz:Plus (Number 2) (Number (-2))"
```

The **bad news**: the code is super duper **gross**

## Lets spot a Pattern

The code is gross because we have these cascading blocks

```haskell
case e1 of
  Error err1 -> Error err1
  Value v1   -> case e2 of
                  Error err2 -> Error err2
                  Value v1   -> Result (v1 + v2)
```

but really both blocks have something **common pattern**

```haskell
case e of
  Error err -> Error err
  Value v   -> {- do stuff with v -}
```

1. Evaluate `e`
2. If the result is an `Error` then _return_ that error.
3. If the result is a `Value v` then _do further processing_ on `v`.

Lets **bottle** that common structure in two functions:

- `>>=` (pronounced _bind_)
- `return` (pronounced _return_)

![Bottling a Magic Pattern](/static/img/fairy.png){#fig:types .align-center width=20%}

```haskell
(>>=) :: Result a -> (a -> Result b) -> Result b
(Error err) >>= _       = Error err
(Value v)   >>= process = process v

return :: a -> Result a
return v = Value v
```

**NOTE:** `return` is _not_ a keyword; it is just the name of a function!

## A Cleaned up Evaluator

The magic bottle lets us clean up our `eval`

```haskell
eval :: Expr -> Result Int
eval (Number n)   = return n
eval (Plus e1 e2) = eval e1 >>= \v1 ->
                      eval e2 >>= \v2 ->
                        return (v1 + v2)
eval (Div e1 e2)  = eval e1 >>= \v1 ->
                      eval e2 >>= \v2 ->
                        if v2 == 0
                          then Error ("yikes dbz:" ++ show e2)
                          else return (v1 `div` v2)
```

**The gross _pattern matching_ is all hidden inside `>>=`**

Notice the `>>=` takes *two* inputs of type:

- `Result Int`        (e.g. `eval e1` or `eval e2`)
- `Int -> Result Int` (e.g. The _processing_ function that takes the `v` and does stuff with it)

In the above, the processing functions are written using `\v1 -> ...` and `\v2 -> ...`

**NOTE:** It is _crucial_ that you understand what the code above
is doing, and why it is actually just a "shorter" version of the
(gross) nested-case-of `eval`.

## A Class for `>>=`

Like `fmap` or `show` or `jval` or `==`, or `<=`, 
the `>>=` operator is useful across **many** types! 

Lets capture it in an interface/typeclass:

```haskell
class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

Notice how the definitions for `Result` fit the above, with `m = Result` 

```haskell
instance Monad Result where
  (>>=) :: Either a -> (a -> Either b) -> Either b
  (Error err) >>= _       = Error err
  (Value v)   >>= process = process v

  return :: a -> Result a
  return v = Value v
```

## Syntax for `>>=`

In fact `>>=` is *so* useful there is special syntax for it.

Instead of writing

```haskell
e1 >>= \v1 ->
  e2 >>= \v2 ->
    e3 >>= \v3 ->
      e
```

you can write

```haskell
do v1 <- e1
   v2 <- e2
   v3 <- e3
   e
...
```

Thus, we can further simplify our `eval` to:

```haskell
eval :: Expr -> Result Int
eval (Number n)   = return n
eval (Plus e1 e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return (v1 + v2)
eval (Div e1 e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       if v2 == 0
                         then Error ("yikes dbz:" ++ show e2)
                         else return (v1 `div` v2)
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Writing Applications

In most language related classes, we _start_ with a "Hello world!" program.

With 130, we will _end_ with it.

<!-- 
For example, in Python you may write:

```python
def main():
    print "hello, world!"

main()
```

and then you can run it:

```sh
$ python hello.py
hello world!
```
-->

## Purity and the Immutability Principle

Haskell is a **pure** language. Not a _value_ judgment, but a precise _technical_ statement:

**The "Immutability Principle":**

- A function must _always_ return the same output for a given input

- A function's behavior should _never change_

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## No Side Effects

![](/static/img/trinity.png){#fig:types .align-center width=60%}

Haskell's most radical idea: `expression ==> value`

- When you evaluate an expression you get a value and **nothing else happens**

Specifically, evaluation must not have an **side effects**

- _change_ a global variable or

- _print_ to screen or

- _read_ a file or

- _send_ an email or

- _launch_ a missile.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Purity

Means _functions may depend only on their inputs_

- i.e. **functions should give the same output for the same input every time.**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## But... how to write "Hello, world!"

But, we _want_ to ...

- print to screen
- read a file
- send an email

A language that only lets you write `factorial` and `fibonacci` is ... _not very useful_!

Thankfully, you _can_ do all the above via a very clever idea: `Recipe`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recipes

[This analogy is due to Joachim Brietner][brietner]

Haskell has a special type called `IO` -- which you can think of as `Recipe` 

```haskell
type Recipe a = IO a
```

A _value_ of type `Recipe a` is

- a **description** of an effectful computations

- when **when executed** (possibly) perform some effectful I/O operations to

- **produce** a value of type `a`.

## Recipes have No Effects

A value of type `Recipe a` is

- Just a **description** of an effectful computation

- An inert, perfectly safe thing with **no effects**.

![Cake vs. Recipe](/static/img/cake.png){#fig:types .align-center width=80%}

**(L)** chocolate _cake_, **(R)** a _sequence of instructions_ on how to make a cake.

They are different (_hint_: only one of them is delicious.)

Merely having a `Recipe Cake` has no effects: holding the recipe

- Does not make your oven _hot_

- Does not make your your floor _dirty_

## Executing Recipes

There is **only one way** to execute a `Recipe a`

Haskell looks for a special value

```haskell
main :: Recipe ()
```

The value associated with `main` is handed to the **runtime system and executed**

![Baker Aker](/static/img/baker-aker.jpg){#fig:types .align-center width=70%}

The Haskell runtime is a _master chef_ who is the only one allowed to cook!

## How to write an App in Haskell

Make a `Recipe ()` that is handed off to the master chef `main`.

- `main` can be arbitrarily complicated

- will be composed of _many smaller_ recipes

## Hello World


```haskell
putStrLn :: String -> Recipe ()
```

The function `putStrLn`

- takes as input a `String`
- returns as output a `Recipe ()`

`putStrLn msg` is a `Recipe ()` _when executed_ prints out `msg` on the screen.

```haskell
main :: Recipe ()
main = putStrLn "Hello, world!"
```

... and we can compile and run it

```sh
$ ghc --make hello.hs
$ ./hello
Hello, world!
```

## QUIZ: Combining Recipes

Next, lets write a program that prints multiple things:

```haskell
main :: IO ()
main = combine (putStrLn "Hello,") (putStrLn "World!")

-- putStrLn :: String -> Recipe ()
-- combine  :: ???
```

What must the _type_ of `combine` be?

```haskell
{- A -} combine :: () -> () -> ()
{- B -} combine :: Recipe () -> Recipe () -> Recipe ()
{- C -} combine :: Recipe a  -> Recipe a  -> Recipe a
{- D -} combine :: Recipe a  -> Recipe b  -> Recipe b
{- E -} combine :: Recipe a  -> Recipe b  -> Recipe a
```

<br>
<br>
<br>
<br>

## Using Intermediate Results

Next, lets write a program that

1. **Asks** for the user's `name` using

```haskell
    getLine :: Recipe String
```

2. **Prints** out a greeting with that `name` using

```haskell
    putStrLn :: String -> Recipe ()
```

**Problem:** How to pass the **output** of _first_ recipe into the _second_ recipe?

<br>
<br>
<br>
<br>

## QUIZ: Using Yolks to Make Batter

Suppose you have two recipes

```haskell
crack     :: Recipe Yolk
eggBatter :: Yolk -> Recipe Batter
```

and we want to get 

```haskell
mkBatter :: Recipe Batter
mkBatter = crack `combineWithResult` eggBatter
```

What must the type of `combineWithResult` be?

```haskell
{- A -} Yolk -> Batter -> Batter
{- B -} Recipe Yolk -> (Yolk  -> Recipe Batter) -> Recipe Batter
{- C -} Recipe a    -> (a     -> Recipe a     ) -> Recipe a
{- D -} Recipe a    -> (a     -> Recipe b     ) -> Recipe b
{- E -} Recipe Yolk -> (Yolk  -> Recipe Batter) -> Recipe ()
```

<br>
<br>
<br>
<br>

## Looks Familiar

Wait a bit, the signature looks familiar!

```haskell
combineWithResult :: Recipe a -> (a -> Recipe b) -> Recipe b
```

Remember this

```haskell
(>>=)             :: Result a -> (a -> Result b) -> Result b
```

## `Recipe` is an instance of `Monad`

In fact, in the standard library

```haskell
instance Monad Recipe where
  (>>=) = {-... combineWithResult... -}
```

So we can put this together with `putStrLn` to get:

```haskell
main :: Recipe ()
main = getLine >>= \name -> putStrLn ("Hello, " ++ name ++ "!")
```

or, using `do` notation the above becomes

```haskell
main :: Recipe ()
main = do name <- getLine
          putStrLn ("Hello, " ++ name ++ "!")
```

**Exercise** 

1. _Compile_ and run to make sure its ok!
2. _Modify_ the above to repeatedly ask for names.
3. _Extend_ the above to print a "prompt" that tells you how many iterations have occurred.

<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Monads are Amazing

Monads have had a _revolutionary_ influence in PL, well beyond Haskell, some recent examples

- **Error handling** in `go` e.g. [1](https://speakerdeck.com/rebeccaskinner/monadic-error-handling-in-go)  and [2](https://www.innoq.com/en/blog/golang-errors-monads/)

- **Asynchrony** in JavaScript e.g. [1](https://gist.github.com/MaiaVictor/bc0c02b6d1fbc7e3dbae838fb1376c80) and [2](https://medium.com/@dtipson/building-a-better-promise-3dd366f80c16)

- **Big data** pipelines e.g. [LinQ](https://www.microsoft.com/en-us/research/project/dryadlinq/) and [TensorFlow](https://www.tensorflow.org/)

<br>
<br>
<br>
<br>
<br>
<br>
<br>

## A Silly App to End CSE 130

Lets write an app called [moo](/static/raw/moo.hs) inspired by [cowsay](https://medium.com/@jasonrigden/cowsay-is-the-most-important-unix-like-command-ever-35abdbc22b7f)

**A Command Line App**

![`moo`](/static/img/moo1.png){#fig:types .align-center width=70%}

**`moo` works with pipes**

![Thanks, and good luck for the final!](/static/img/moo2.png){#fig:types .align-center width=70%}

<!--
![](/static/img/moo3.png){#fig:types .align-center width=70%}

```sh
$ ./moo Jhala, y u no make final easy!

 --------------------------------
< Jhala, y u no make final easy! >
 --------------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```

or even using unix pipes

```txt
$ ./moo Thats all folks, thanks!

 ------------------------------------
< 00-intro.pdf 01-lambda.pdf         >
< 03-datatypes.pdf 04-hof.pdf        >
< 05-environments.pdf 06-parsing.pdf >
< 07-classes.pdf 08-monads.pdf       >
 ------------------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```

Thats all, folks.

-->


[brietner]: https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html