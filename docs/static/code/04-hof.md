---
title: Higher-Order Functions 
date: 2018-04-23
headerImg: books.jpg
---

## Plan for this week

**Last week:**

  * user-defined *data types*
    
  * manipulating data-types with *pattern matching* and *recursion* 

  * how to make recursive functions more efficient with *tail recursion*
    
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>    

## The long arc of history

Pattern matching is a *very* old PL idea ...  

- Variants of LISP from 1970 by [Fred McBride](https://personal.cis.strath.ac.uk/conor.mcbride/FVMcB-PhD.pdf)

... but will finally be added to Python 3.10

- https://www.python.org/dev/peps/pep-0622/

```python
def make_point_3d(pt):
    match pt:
        case (x, y):
            return Point3d(x, y, 0)
        case (x, y, z):
            return Point3d(x, y, z)
        case Point2d(x, y):
            return Point3d(x, y, 0)
        case Point3d(_, _, _):
            return pt
        case _:
            raise TypeError("not a point we support")
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>    

## Plan for this week

**Last week:**

  * user-defined *data types*
    
  * manipulating data-types with *pattern matching* and *recursion* 

  * how to make recursive functions more efficient with *tail recursion*
  
**This week:**

  * code reuse with *higher-order functions* (HOFs)

  * some useful HOFs: `map`, `filter`, and `fold`
    
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>    

## Recursion is good... 

- Recursive code mirrors recursive data

    - Base constructor -> Base case 
    - Inductive constructor -> Inductive case (with recursive call)

- But it can get kinda repetitive!  

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Example: evens

Let's write a function `evens`:

```haskell
-- evens []        ==> []
-- evens [1,2,3,4] ==> [2,4]
```

```haskell
evens       :: [Int] -> [Int]
evens []     = ... 
evens (x:xs) = ...
```

<br>
<br>
<br>
<br>
<br>
<br>

## Example: four-letter words

Let's write a function `fourChars`:

```haskell
-- fourChars [] ==> []
-- fourChars ["i","must","do","work"] ==> ["must","work"]
```

```haskell
fourChars :: [String] -> [String]
fourChars []     = ... 
fourChars (x:xs) = ...
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Yikes! Most Code is the Same!

Lets rename the functions to `foo`:

```haskell
foo []            = []
foo (x:xs)
  | x mod 2 == 0  = x : foo xs
  | otherwise     =     foo xs

foo []            = []
foo (x:xs)
  | length x == 4 = x : foo xs
  | otherwise     =     foo xs
```

Only difference is **condition**

- `x mod 2 == 0` vs `length x == 4`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Moral of the day

<br>

**D.R.Y.** Don't Repeat Yourself!

<br>

Can we 

* *reuse* the general pattern and

* *plug-in* the custom condition?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Higher-Order Functions

General **Pattern**
  
  - expressed as a *higher-order function*
  - takes plugin operations as *arguments* 

Specific **Operation**

  - passed in as an argument to the HOF


<br>
<br>
<br>
<br>
<br>
<br>

## The "filter" pattern

![The `filter` Pattern](/static/img/filter-pattern.png)

General Pattern

- HOF `filter` 
- Recursively traverse list and pick out elements that satisfy a predicate

Specific Operations

- Predicates `isEven` and `isFour`

![`filter` instances](/static/img/filter-pattern-instance.png)

**Avoid duplicating code!**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ: What is the type of `filter`?

```haskell
-- evens [1,2,3,4] ==> [2,4]
evens :: [Int] -> [Int]
evens xs = filter isEven xs
  where
    isEven :: Int -> Bool
    isEven x  =  x `mod` 2 == 0

-- fourChars ["i","must","do","work"] ==> ["must","work"]
fourChars :: [String] -> [String]
fourChars xs = filter isFour xs
  where
    isFour :: String -> Bool
    isFour x  =  length x == 4
```

So what's the type of `filter`?

```haskell
{- A -} filter :: (Int -> Bool) -> [Int] -> [Int]

{- B -} filter :: (String -> Bool) -> [String] -> [String]

{- C -} filter :: (a -> Bool) -> [a] -> [a]

{- D -} filter :: (a -> Bool) -> [a] -> [Bool]

{- E -} filter :: (a -> b) -> [a] -> [b]
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

    
## Type of `filter` 

```haskell
-- evens [1,2,3,4] ==> [2,4]
evens :: [Int] -> [Int]
evens xs = filter isEven xs
  where
    isEven :: Int -> Bool
    isEven x  =  x `mod` 2 == 0

-- fourChars ["i","must","do","work"] ==> ["must","work"]
fourChars :: [String] -> [String]
fourChars xs = filter isFour xs
  where
    isFour :: String -> Bool
    isFour x  =  length x == 4
```

For *any* type `a`

- **Input** a _predicate_ `a -> Bool` and _collection_ `[a]` 
- **Output** a (smaller) _collection_ `[a]`


```haskell
filter :: (a -> Bool) -> [a] -> [a]
```

`filter` *does not care* what the list elements are

* as long as the predicate can handle them
  
`filter` is **polymorphic** (generic) in the type of list elements

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>





## Example: ALL CAPS!

Lets write a function `shout`:

```haskell
-- shout []                    ==> []
-- shout ['h','e','l','l','o'] ==> ['H','E','L','L','O'] 
```

```haskell
shout :: [Char] -> [Char]
shout []     = ...
shout (x:xs) = ... 
```

<br>
<br>
<br>
<br>
<br>
<br>

## Example: squares

Lets write a function `squares`:

```haskell
-- squares []        ==> []
-- squares [1,2,3,4] ==> [1,4,9,16] 
```

```haskell
squares :: [Int] -> [Int]
squares []     = ...
squares (x:xs) = ...
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Yikes, Most Code is the Same

Lets rename the functions to `foo`:

```haskell
-- shout
foo []     = []
foo (x:xs) = toUpper x : foo xs

-- squares
foo []     = []
foo (x:xs) = (x * x)   : foo xs
```

<br>
<br>

Lets **refactor** into the **common pattern**

```haskell
pattern = ...
```

<br>
<br>
<br>
<br>
<br>
<br>

## The "map" pattern

![The `map` Pattern](/static/img/map-pattern.png)

General Pattern

- HOF `map`
- Apply a transformation `f` to each element of a list

Specific Operations

- Transformations `toUpper` and `\x -> x * x`

<br>
<br>
<br>
<br>

```haskell
map f []     = []
map f (x:xs) = f x : map f xs
```

Lets refactor `shout` and `squares`

```haskell
shout   = map ...

squares = map ...
```

<br>
<br>
<br>
<br>

![`map` instances](/static/img/map-pattern-instance.png)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ

What is the type of `map`?

```haskell
map f []     = []
map f (x:xs) = f x : map f xs
```

**(A)** `(Char -> Char) -> [Char] -> [Char]`

**(B)** `(Int -> Int) -> [Int] -> [Int]`

**(C)** `(a -> a) -> [a] -> [a]`

**(D)** `(a -> b) -> [a] -> [b]`

**(E)** `(a -> b) -> [c] -> [d]`

    
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

```haskell
-- For any types `a` and `b`
--   if you give me a transformation from `a` to `b`
--   and a list of `a`s,
--   I'll give you back a list of `b`s 
map :: (a -> b) -> [a] -> [b]
```

<br>

**Type says it all!**

* The only meaningful thing a function of this type can do is apply its first argument to elements of the list

* Hoogle it!

<br>

Things to try at home:

  * can you write a function `map' :: (a -> b) -> [a] -> [b]` whose behavior is different from `map`?
  
  * can you write a function `map' :: (a -> b) -> [a] -> [b]`
    such that `map' f xs` returns a list whose elements are not in `map f xs`?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ

What is the value of `quiz`?

```haskell
map :: (a -> b) -> [a] -> [b]

quiz = map (\(x, y) -> x + y) [1, 2, 3]
```

**(A)** `[2, 4, 6]`

**(B)** `[3, 5]`

**(C)** Syntax Error

**(D)** Type Error

**(E)** None of the above

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Don't Repeat Yourself

Benefits of **factoring** code with HOFs:

- Reuse iteration pattern

    - think in terms of standard patterns
    
    - less to write
  
    - easier to communicate

- Avoid bugs due to repetition

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recall: length of a list

```haskell
-- len []      ==> 0
-- len ["carne","asada"] ==> 2
len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



## Recall: summing a list

```haskell
-- sum []      ==> 0
-- sum [1,2,3] ==> 6
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Example: string concatenation

Let's write a function `cat`:

```haskell
-- cat [] ==> ""
-- cat ["carne","asada","torta"] ==> "carneasadatorta"
cat :: [String] -> String
cat []     = ...
cat (x:xs) = ...
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Can you spot the pattern?

```haskell
-- len
foo []     = 0
foo (x:xs) = 1 + foo xs

-- sum
foo []     = 0
foo (x:xs) = x + foo xs

-- cat
foo []     = ""
foo (x:xs) = x ++ foo xs
```

<br>

```haskell
pattern = ...
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The "fold-right" pattern

![The `foldr` Pattern](/static/img/foldr-pattern.png)

General Pattern

- Recurse on tail
- Combine result with the head using some binary operation

<br>
<br>
<br>
<br>

```haskell
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)
```

<br>
<br>

Let's refactor `sum`, `len` and `cat`: 

```haskell
sum = foldr ...  ...

cat = foldr ...  ...

len = foldr ...  ...
```

Factor the recursion out!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


![`foldr` instances](/static/img/foldr-pattern-instance.png)

You can write it more clearly as

```haskell
sum = foldr (+) 0

cat = foldr (++) ""
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## The "fold-right" pattern

```haskell
foldr f b [a1, a2, a3, a4]
  ==> f a1 (foldr f b [a2, a3, a4])
  ==> f a1 (f a2 (foldr f b [a3, a4]))
  ==> f a1 (f a2 (f a3 (foldr f b [a4])))
  ==> f a1 (f a2 (f a3 (f a4 (foldr f b []))))
  ==> f a1 (f a2 (f a3 (f a4 b)))
```

Accumulate the values from the **right**

For example:

```haskell
foldr (+) 0 [1, 2, 3, 4]
  ==> 1 + (foldr (+) 0 [2, 3, 4])
  ==> 1 + (2 + (foldr (+) 0 [3, 4]))
  ==> 1 + (2 + (3 + (foldr (+) 0 [4])))
  ==> 1 + (2 + (3 + (4 + (foldr (+) 0 []))))
  ==> 1 + (2 + (3 + (4 + 0)))
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ

What does this evaluate to?

```haskell
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

quiz = foldr (\x v -> x : v) [] [1,2,3]
```

**(A)** Type error

**(B)** `[1,2,3]`

**(C)** `[3,2,1]`

**(D)** `[[3],[2],[1]]`

**(E)** `[[1],[2],[3]]`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

```haskell
foldr (:) [] [1,2,3]
  ==> (:) 1 (foldr (:) [] [2, 3])
  ==> (:) 1 ((:) 2 (foldr (:) [] [3]))
  ==> (:) 1 ((:) 2 ((:) 3 (foldr (:) [] [])))
  ==> (:) 1 ((:) 2 ((:) 3 []))
  ==  1 : (2 : (3 : []))
  ==  [1,2,3]
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## QUIZ

What is the most general type of `foldr`?

```haskell

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)
```

**(A)** `(a -> a -> a) -> a -> [a] -> a`

**(B)** `(a -> a -> b) -> a -> [a] -> b`

**(C)** `(a -> b -> a) -> b -> [a] -> b`

**(D)** `(a -> b -> b) -> b -> [a] -> b`

**(E)** `(b -> a -> b) -> b -> [a] -> b`

<br>

(I) final
    
    *Answer:* D

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Tail Recursive Fold

```haskell
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)
```

Is `foldr` **tail recursive**?

(I) final
    
    *Answer:* No! It calls the binary operations on the results of the recursive call

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## What about tail-recursive versions?

Let's write tail-recursive `sum`!

```haskell
sumTR :: [Int] -> Int
sumTR = ...
```
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Lets run `sumTR` to see how it works

```haskell
sumTR [1,2,3]
  ==> helper 0 [1,2,3]
  ==> helper 1   [2,3]    -- 0 + 1 ==> 1
  ==> helper 3     [3]    -- 1 + 2 ==> 3
  ==> helper 6      []    -- 3 + 3 ==> 6 
  ==> 6
```

**Note:** `helper` directly returns the result of recursive call! 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Let's write tail-recursive `cat`!

```haskell
catTR :: [String] -> String 
catTR = ...
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Lets run `catTR` to see how it works

```haskell
catTR                 ["carne", "asada", "torta"]

  ==> helper ""       ["carne", "asada", "torta"]

  ==> helper "carne"           ["asada", "torta"]

  ==> helper "carneasada"               ["torta"]

  ==> helper "carneasadatorta"                 []

  ==> "carneasadatorta"
```

**Note:** `helper` directly returns the result of recursive call! 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Can you spot the pattern?

```haskell
-- sumTR
foo xs                = helper 0 xs
  where
    helper acc []     = acc
    helper acc (x:xs) = helper (acc + x) xs


-- catTR
foo xs                = helper "" xs
  where
    helper acc []     = acc
    helper acc (x:xs) = helper (acc ++ x) xs
```

<br>

```haskell
pattern = ...
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The "fold-left" pattern

![The `foldl` Pattern](/static/img/foldl-pattern.png)

General Pattern

- Use a helper function with an extra accumulator argument
- To compute new accumulator, combine current accumulator with the head using some binary operation

<br>
<br>
<br>
<br>

```haskell
foldl f b xs          = helper b xs
  where
    helper acc []     = acc
    helper acc (x:xs) = helper (f acc x) xs
```

<br>
<br>

Let's refactor `sumTR` and `catTR`: 

```haskell
sumTR = foldl ...  ...

catTR = foldl ...  ...
```

Factor the tail-recursion out!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## QUIZ

What does this evaluate to?

```haskell
foldl f b xs          = helper b xs
  where
    helper acc []     = acc
    helper acc (x:xs) = helper (f acc x) xs

quiz = foldl (\xs x -> x : xs) [] [1,2,3]
```

<br>

**(A)** Type error

**(B)** `[1,2,3]`

**(C)** `[3,2,1]`

**(D)** `[[3],[2],[1]]`

**(E)** `[[1],[2],[3]]`

<br>

(I) final
    
    *Answer:* C

```
foldl f b (x1: x2: x3 : [])
  ==> helper b (x1: x2: x3 : [])
  ==> helper (f x1 b)  (x2: x3 : [])
  ==> helper (f x2 (f x1 b))  (x3 : [])
  ==> helper (f x3 (f x2 (f x1 b)))  []
  ==> ( x3 :  (x2 : (x1 : [])))
```


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The "fold-left" pattern

```haskell
foldl f b                     [x1, x2, x3, x4]
  ==> helper b                [x1, x2, x3, x4]
  ==> helper (f b x1)             [x2, x3, x4]
  ==> helper (f (f b x1) x2)          [x3, x4]
  ==> helper (f (f (f b x1) x2) x3)       [x4]
  ==> helper (f (f (f (f b x1) x2) x3) x4)  []
  ==> (f (f (f (f b x1) x2) x3) x4)
```

Accumulate the values from the **left**

For example:

```haskell
foldl (+) 0                   [1, 2, 3, 4]
  ==> helper 0                [1, 2, 3, 4]
  ==> helper (0 + 1)             [2, 3, 4]
  ==> helper ((0 + 1) + 2)          [3, 4]
  ==> helper (((0 + 1) + 2) + 3)       [4]
  ==> helper ((((0 + 1) + 2) + 3) + 4)  []
  ==> ((((0 + 1) + 2) + 3) + 4)
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Left vs. Right

```haskell
foldl f b [x1, x2, x3]  ==> f (f (f b x1) x2) x3  -- Left

foldr f b [x1, x2, x3]  ==> f x1 (f x2 (f x3 b))  -- Right
```

For example:

```haskell
foldl (+) 0 [1, 2, 3]  ==> ((0 + 1) + 2) + 3  -- Left

foldr (+) 0 [1, 2, 3]  ==> 1 + (2 + (3 + 0))  -- Right
```

Different types!

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b  -- Left

foldr :: (a -> b -> b) -> b -> [a] -> b  -- Right
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Higher Order Functions

Iteration patterns over collections:

- **Filter** values in a collection given a *predicate*
- **Map** (iterate) a given *transformation* over a collection
- **Fold** (reduce) a collection into a value, given a *binary operation* to combine results

<br>

HOFs can be put into libraries to enable modularity

- Data structure **library** implements `map`, `filter`, `fold` for its collections

    - generic efficient implementation
    
    - generic optimizations: `map f (map g xs) --> map (f.g) xs`
    

- Data structure **clients** use HOFs with specific operations
    
    - no need to know the implementation of the collection 
    
Crucial foundation of 

- "big data" revolution e.g. _MapReduce_, _Spark_, _TensorFlow_

- "web programming" revolution e.g. _Jquery_, _Angular_, _React_  

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

