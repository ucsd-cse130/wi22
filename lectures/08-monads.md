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

## Generalizing `Result` to *Many* Values

We can generalize `Result` to "many" values, using `List`

```haskell
data Result a = Err String | Result a
data List   a = Nil        | Cons   a (List a) 
```

- The `Err` is like `[]` except it has a message too,
- The `tail` of type `(List a)` lets us hold *many* possible `a` values

We can now make a `Monad` instance for lists as

```haskell
instance Monad [] where
  return = returnList 
  (>>=)  = bindList 

returnList :: a -> [a]
returnList x = [x]

bindList :: [a] -> (a -> [b]) -> [b]
bindList []     f = []
bindList (x:xs) f = f x ++ bindList xs f
```

Notice `bindList xs f` is like a `for-loop`:

- **for each** `x` in `xs` we call,
- `f x` to get the results 
- and concatenate _all_ the results

so,

```haskell
bindList [x1,x2,x3,...,xn] f ==>
  f x1 ++ f x2 ++ f x3 ++ ... ++ f xn
```

This has some fun consequences!

```haskell
silly xs = do
  x <- xs
  return (x * 10)
```

produces the result

```haskell
-- >>> silly [1,2,3]
-- [10,20,30]
```

and

```haskell
foo xs ys = do
  x <- xs
  y <- ys
  return (x, y)
```

produces the result

```haskell
-- >>> foo ["1", "2", "red", "blue"] ["fish"]
-- [("1","fish"),("2","fish"),("red","fish"),("blue","fish")]
```

behaves like Python's

```python
for x in xs:
  for y in ys:
    yield (x, y)
```

## EXERCISE 

Fill in the blanks to implement `mMap` (i.e. `map` using monads) 

```haskell
mMap :: (a -> b) -> [a] -> [b]
mMap f xs = do
  _fixme
```

## EXERCISE 

Fill in the blanks to implement `mFilter` (i.e. `filter` using monads)

```haskell
mFilter :: (a -> Bool) -> [a] -> [a]
mFilter f xs = do
  _fixme
```

