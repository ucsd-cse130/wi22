---
title: A crash course in Haskell
date: 2019-04-15
headerImg: books.jpg
---

## Functions and Programming

<br>    
<br>    

![](/static/img/carmack-tweet-function.png){#fig:landin .align-center width=60%}

<br>
<br>
<br>
<br>
<br>

## What is Haskell?

<br>

A **typed**, **lazy**, **purely functional** programming language

<br>

Haskell = $\lambda$-calculus ++

  + better syntax
  + types
  + built-in features
    - booleans, numbers, characters
    - records (tuples)
    - lists
    - recursion
    - ...
    
<br>
<br>
<br>
<br>
<br>
<br>    
    
## Why Haskell?

Haskell programs tend to be *simple* and *correct*   

### QuickSort in Haskell

```haskell
sort        :: (Ord a) => [a] -> [a]
sort []     = []
sort (x:xs) = sort ls ++ [x] ++ sort rs
  where
    ls      = [ l | l <- xs, l <= x ]
    rs      = [ r | r <- xs, x <  r ]
```


### Goals for this week

1. Understand the code above
2. Understand what **typed**, **lazy**, and **purely functional** means (and why it's cool)

<br>
<br>
<br>
<br>
<br>
<br>

## Haskell vs $\lambda$-calculus: similarities

### (1) Programs

A program is an **expression** (*not* a sequence of statements)

It **evaluates** to a value (it *does not* perform actions)

  * **$\lambda$**:

    ```
    (\x -> x) apple     -- =~> apple
    ```

  * **Haskell**:
  
    ```
    (\x -> x) "apple"   -- =~> "apple"
    ```

### (2) Functions    
      
Functions are *first-class values*:

* can be *passed as arguments* to other functions
* can be *returned as results* from other functions
* can be *partially applied* (arguments passed *one at a time*)

(I) lecture
 
    ```haskell
    (\x -> (\y -> x (x y))) (\z -> z + 1) 0   -- =~> ???
    ```
    
(I) final    

    ```haskell
    (\x -> (\y -> x (x y))) (\z -> z + 1) 0   -- =~> 2
    ```


*But:* unlike $\lambda$-calculus, not everything is a function!

     
### (3) Top-level bindings

Like in Elsa, we can *name* terms to use them later
 
**Elsa**:

```haskell
let T    = \x y -> x
let F    = \x y -> y

let PAIR = \x y -> \b -> ITE b x y
let FST  = \p -> p T
let SND  = \p -> p F

eval fst:
 FST (PAIR apple orange)
 =~> apple
```

**Haskell**:

```haskell
haskellIsAwesome = True

pair = \x y -> \b -> if b then x else y
fst = \p -> p haskellIsAwesome
snd = \p -> p False

-- In GHCi:
> fst (pair "apple" "orange")   -- "apple"
```   
    
The names are called **top-level variables**

Their definitions are called **top-level bindings**

<br>
<br>
<br>
<br>
<br>
<br>
    
## Better Syntax: Equations and Patterns

You can define function bindings using **equations**:

```haskell
pair x y b = if b then x else y -- same as: pair = \x y b -> ...
fst p      = p True             -- same as: fst = \p -> ...
snd p      = p False            -- same as: snd = \p -> ...
```
<br>
<br>
<br>
<br>
<br>
<br>

A *single* function binding can have *multiple* equations with different **patterns** of parameters:

```haskell
pair x y True  = x  -- If 3rd arg matches True,
                    -- use this equation;
pair x y False = y  -- Otherwise, if 3rd arg matches False,
                    -- use this equation.
```

At run time, the first equation whose pattern matches the actual arguments is chosen

For now, a **pattern** is:
 
  * a *variable* (matches any value)
  
  * or a *value* (matches only that value)

<br>
<br>

Same as:

```haskell
pair x y True  = x  -- If 3rd arg matches True,
                    -- use this equation;
pair x y b     = y  -- Otherwise, use this equation.
```

<br>
<br>

Same as:

```haskell
pair x y True  = x
pair x y _     = y
```

<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ

Which of the following definitions of `pair` is **incorrect**?

**A.** `pair x y = \b -> if b then x else y`

**B.** `pair x = \y b -> if b then x else y`

**C.**
```haskell
pair x _ True  = x
pair _ y _     = y
```

**D.**
```haskell
pair x y b     = x
pair x y False = y
```

**E.**  all of the above

<br>

(I) final

    _Answer:_ **D**

<br>
<br>
<br>
<br>
<br>

## Equations with guards

An equation can have multiple guards (Boolean expressions):

```haskell
cmpSquare x y  |  x > y*y   =  "bigger :)"
               |  x == y*y  =  "same :|"
               |  x < y*y   =  "smaller :("
```

Same as:

```haskell
cmpSquare x y  |  x > y*y   =  "bigger :)"
               |  x == y*y  =  "same :|"
               |  otherwise =  "smaller :("
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

## Recusion

Recursion is built-in, so you can write:

```haskell
sum n = if n == 0 
          then 0 
          else n + sum (n - 1)
```

or you can write:

```haskell
sum 0 = 0
sum n = n + sum (n - 1)
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The scope of variables

Top-level variable have **global** scope,
so you can write:

```haskell
message = if haskellIsAwesome          -- this var defined below
            then "I love CSE 130"
            else "I'm dropping CSE 130"
            
haskellIsAwesome = True
```

<br>
<br>

Or you can write:

```haskell
-- What does f compute?
f 0 = True
f n = g (n - 1) -- mutual recursion!

g 0 = False
g n = f (n - 1) -- mutual recursion!
```

(I) final

    Answer: `f` is `isEven`, `g` is `isOdd` 

<br>
<br>
<br>

Is this allowed?

```haskell
haskellIsAwesome = True

haskellIsAwesome = False -- changed my mind
```

(I) final

    Answer: no, a variable can be defined once per scope; no mutation!

<br>
<br>
<br>
<br>

### Local variables

You can introduce a *new* (local) scope using a `let`-expression:

```haskell
sum 0 = 0
sum n = let n' = n - 1          
        in n + sum n'  -- the scope of n' is the term after in
```

<br>
<br>
<br>

Syntactic sugar for nested `let`-expressions:

```haskell
sum 0 = 0
sum n = let 
          n'   = n - 1
          sum' = sum n'
        in n + sum'
```

<br>
<br>
<br>

If you need a variable whose scope is an equation, use the `where` clause instead:

```haskell
cmpSquare x y  |  x > z   =  "bigger :)"
               |  x == z  =  "same :|"
               |  x < z   =  "smaller :("
  where z = y*y
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Types

<br>
<br>
<br>
<br>

What would *Elsa* say?

```haskell
let WEIRDO = ONE ZERO
```

<br>

(I) final

    Answer: Nothing. When evaluated will crunch to something nonsensical.
    $lambda$-calculus is **untyped**.

<br>
<br>
<br>

What would *Python* say?

```python
def weirdo():
  return 0(1)
```

<br>

(I) final

    Answer: Nothing. When evaluated will cause a run-time error.
    Python is **dynamically typed**.

<br>
<br>
<br>

What would *Java* say?

```java
void weirdo() {
  int zero;
  zero(1);
}
```

<br>

(I) final

    Answer: Java compiler will reject this.
    Java is **statically typed**.

<br>
<br>
<br>
<br>
<br>
<br>




## Types 

![](/static/img/trinity.png){#fig:types .align-center width=60%}

In *Haskell* every expression either

- **ill-typed** and _rejected at compile time_ or
- **has a type** and can be _evaluated_ to obtain
_ **a value** of the same type.

Ill-typed* expressions are rejected statically at *compile-time*, before execution starts

* **like**   in Java
* **unlike** $\lambda$-calculus or Python ... 

```haskell
weirdo = 1 0     -- rejected by GHC
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



### Why are types good?

* Helps with program *design*
* Types are *contracts* (ignore ill-typed inputs!)
* Catches errors *early*
* Allows compiler *to generate code*
* Enables compiler *optimizations*
  
<br>
<br>
<br>
<br>
<br>
<br>



## Type annotations

You can annotate your bindings with their types using `::`, like so:

```haskell
-- | This is a Boolean:
haskellIsAwesome :: Bool            
haskellIsAwesome = True

-- | This is a string
message :: String
message = if haskellIsAwesome
            then "I love CSE 130"
            else "I'm dropping CSE 130"
            
-- | This is a word-size integer
rating :: Int
rating = if haskellIsAwesome then 10 else 0

-- | This is an arbitrary precision integer
bigNumber :: Integer
bigNumber = factorial 100
```

If you omit annotations, GHC will infer them for you

  * Inspect types in GHCi using `:t`
  * You should annotate all top-level bindings anyway! (Why?)

<br>
<br>
<br>
<br>
<br>
<br>

## Function Types

Functions have **arrow types**:

* `\x -> e` has type `A -> B`
* if `e` has type `B` assuming `x` has type `A`

For example:

(I) lecture
 
    ```haskell
    > :t (\x -> if x then `a` else `b`)  -- ???
    ```    
    
(I) final    

    ```haskell
    > :t (\x -> if x then `a` else `b`)
    (\x -> if x then `a` else `b`) :: Bool -> Char
    ```

<br>
<br>
<br>
<br>

## Always annotate your function bindings

First understand *what the function does*

- Before you think about *how to do it*

```haskell
sum :: Int -> Int
sum 0 = 0
sum n = n + sum (n - 1)
```

<br>
<br>
<br>
<br>

## When you have *multiple arguments

For example

```haskell
add3 :: Int -> (Int -> (Int -> Int))
add3 x y z = x + y + z
```

why? because the above is the same as:

```haskell
add3 :: Int -> (Int -> (Int -> Int))
add3 = \x -> (\y -> (\z -> x + y + z))
```

however, as with the lambdas, the `->` **associates to the right** so we will just write:

```haskell
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
``` 

<br>
<br>
<br>
<br>
<br>
<br>

<!-- 
## QUIZ

Suppose `pair :: String -> String -> Bool -> String`, what is the type of:

```haskell
(pair "apple" "orange")
```

**A.** Syntax error

**B.** The term is ill-typed

**C.** `String`

**D.** `Bool -> String`

**E.** `String -> String -> Bool -> String`

<br>

(I) final

    _Answer:_ **D**

<br>
<br>
<br>
<br>
<br>
<br>
<br>

-->

## Lists

A list is

  * either an *empty list*
    
    `[]       -- pronounced "nil"`
    
  * or a *head element* attached to a *tail list* 
  
    `h : t     -- pronounced "h cons t"`
    
<br>
<br>    
  
Examples:

```haskell
[]                -- A list with zero elements

1 : []            -- A list with one element: 1

(:) 1 []          -- As above: for any infix op, `x op y` is same as `(op) x y`

1:(2:(3:(4:[])))  -- A list with four elements: 1, 2, 3, 4

1:2:3:4:[]        -- Same thing (: is right associative)

[1,2,3,4]         -- Same thing (syntactic sugar)
```  

<br>
<br>
<br>
<br>

### Terminology: constructors and values

`[]` and `(:)` are called the *list* **constructors**

We've seen constructors before:

* `True` and `False` are `Bool` constructors
* `0`, `1`, `2` are ... well, you can think of them as `Int` constructors
  - The `Int` constructors don't take any parameters, we just called them *values*

In general, a **value** is a constructor applied to **other values**

* examples above are *list* values

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The Type of a List

A list has type `[Thing]` if each of its elements has type `Thing`

Examples:

```haskell
intList :: [Int]
intList = [1,2,3,4]

boolList :: [Bool]
boolList = [True, False, True]

strList :: [String]
strList = ["nom", "nom", "burp"]
```

<br>
<br>
<br>
<br>
<br>
<br>



<!-- 

(I) lecture
 
    ```haskell
    -- myList' :: ??
    myList' = ['h', 'e', 'l', 'l', 'o']
    ```
    
(I) final

    ```haskell
    myList' :: [Char]                   -- or :: String
    myList' = ['h', 'e', 'l', 'l', 'o'] -- or = "hello"
    ```

<br>

(I) lecture
 
    ```haskell
    -- myList'' :: ???
    myList'' = [1, 'h']
    ```
    
(I) final    

    ```haskell
    -- myList'' :: Type error: elements have different types!
    myList'' = [1, 'h']
    ```
    
<br>

(I) lecture
 
    ```haskell
    -- myList''' :: ???
    myList''' = []
    ```      
    
(I) final    

    ```haskell
    myList''' :: [t] -- Generic: works for any type t!
    myList''' = []
    ```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

--> 

## Lets write some Functions


[A Recipe](https://www.htdp.org/)

**Step 1: Write some tests**

**Step 2: Write the type**

**Step 3: Write the code**



## Functions on lists: range

**1. Tests** 

```haskell
-- >>> ???
```

**2. Type** 

```haskell
range :: ???
```

**3. Code**

```haskell
range = ???
```

<!-- 
(I) lecture
    ```haskell
    -- | List of integers from n upto m
    upto :: Int -> Int -> [Int]
    upto n m = ???
    ```
    
(I) final    

    ```haskell
    -- | List of integers from n upto m
    upto :: Int -> Int -> [Int]
    upto n m
      | n > m     = []
      | otherwise = n : (upto (n + 1) m)
    ```
-->

<br>
<br>
<br>
<br>
<br>

## Syntactic Sugar for Ranges

There's also syntactic sugar for this!

```haskell
[1..7]    -- [1,2,3,4,5,6,7]
[1,3..7]  -- [1,3,5,7]
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Functions on lists: length

**1. Tests** 

```haskell
-- >>> ???
```

**2. Type** 

```haskell
len :: ???
```

**3. Code**

```haskell
len = ???
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

## Pattern matching on lists

```haskell
-- | Length of the list
len :: [Int] -> Int
len []     = 0
len (_:xs) = 1 + len xs
```

<br>
<br>

~~A pattern is either a *variable* (incl. `_`) or a *value*~~

A pattern is 

  * either a *variable* (incl. `_`)
  * or a *constructor* applied to other *patterns*
  
<br>
<br>  

**Pattern matching** attempts to match *values* against *patterns* and, 
if desired, *bind* variables to successful matches.
  
  
<br>
<br>
<br>
<br>
<br>
<br>


## Functions on lists: `take` 

Let's write a function to `take` first `n` elements of a list `xs`.

**1. Tests** 

```haskell
-- >>> ???
```

**2. Type** 

```haskell
take :: ???
```

**3. Code**

```haskell
take = ???
```

## QUIZ

Which of the following is **not** a pattern?

**A.** `(1:xs)`

**B.** `(_:_:_)`

**C.** `[x]`

**D.** `[1+2,x,y]`

**E.**  all of the above

<br>

(I) final

    _Answer:_ **D** (`1+2` is a function application, not a constructor application)

<br>
<br>
<br>
<br>
<br>  

## Strings are Lists-of-Chars

For example

```haskell
λ> let x = ['h', 'e', 'l', 'l', 'o']
λ> x
"hello"

λ> let y = "hello"

λ> x == y
True

λ> :t x
x :: [Char]

λ> :t y
y :: [Char]
```

## shout Shout SHOUT

How can we convert a string to upper-case, e.g.

```haskell
ghci> shout "like this"
"LIKE THIS"
```

```haskell
shout :: String -> String
shout s = ???
```
<br>

## Some useful library functions

```haskell
-- | Length of the list
length :: [t] -> Int

-- | Append two lists
(++) :: [t] -> [t] -> [t]

-- | Are two lists equal?
(==) :: [t] -> [t] -> Bool
```

<br>

You can search for library functions on [Hoogle](https://www.haskell.org/hoogle/)!

<br>
<br>
<br>
<br>
<br>
<br>

## Tuples

```haskell
myPair :: (String, Int)  -- pair of String and Int
myPair = ("apple", 3)
```

<br>

`(,)` is the *pair constructor*

<br>
<br>

## Field access

Using `fst` and `snd` 

```haskell 
ghci> fst ("apple", 22)
"apple"

ghci> snd ("apple", 22)
22
``` 

## Tuples to pass parameters

```haskell
add2 :: (Int, Int) -> Int
add2 p = fst p + snd p
```

but watch out, `add2` expects a tuple.

```haskell
exAdd2_BAD = add2 10 20      -- type error

exAdd2_OK  = add2 (10, 20)   -- OK!
```

## Tuples and Pattern Matching 

It is often clearer to use *patterns* for tuples, e.g.

```haskell
add2 :: (Int, Int) -> Int
add2 p = let (x, y) = p in
           x + y
```

or equivalently,

```haskell
add2 :: (Int, Int) -> Int
add2 p    = x + y
  where
   (x, y) = p
```

or, best, use the pattern in the parameter,

```haskell
add2 :: (Int, Int) -> Int
add2 (x, y) = x + y
```

<br>
<br>

You can use pattern matching not only in equations,
but also in $\lambda$-bindings and `let`-bindings!

<br>
<br>
<br>
<br>
<br>

### QUIZ: Pattern matching with pairs

Is this pattern matching correct? What does this function do?

```haskell
quiz :: String -> [(String, Int)] -> Int
quiz _ []     = 0
quiz x ((k,v) : ps)
  | x == k    = v
  | otherwise = quiz x ps
```

What is `quiz "dog" [ ("cat", 10), ("dog", 20), ("cat", 30)]` ?

**A.** Type error!

**B.** `0`

**C.** `10`

**D.** `20`

**D.** `30`

<br>
<br>

(I) final

    _Answer:_ a list of pairs represents key-value pairs in a dictionary; 
    `quiz` performs lookup by key, result is `10`.

<br>
<br>
<br>
<br>
<br>

## Generalized Tuples

Can we implement triples like in $\lambda$-calculus?

<br>
<br>
<br>
<br>

Sure! but Haskell has native support for $n$-tuples:

```haskell
myPair   :: (String, Int)
myPair   = ("apple", 3)

myTriple :: (Bool, Int, [Int])
myTriple = (True, 1, [1,2,3])

my4tuple :: (Float, Float, Float, Float)
my4tuple = (pi, sin pi, cos pi, sqrt 2)
```

## The "Empty" Tuple

It also makes sense to have an 0-ary tuple:

```haskell
myUnit :: ()
myUnit = ()
```

often used like `void` in other languages.

<br>
<br>
<br>
<br>
<br>
<br>  

## List comprehensions

A convenient way to construct lists!

## QUIZ

What is the result of evaluating:

```haskell
quiz = [ 10 * i | i <- [0,1,2,3,4,5]]
```

**A.** Infinite loop
**B.** `[]`
**C.** `[0, 10, 20, 30, 40, 50]`
**D.** `150`
**E.** Type error

<br>
<br>
<br>
<br>
<br>  

## Comprehensions and Ranges

Recall you can *enumerate* ranges as

```haskell
ghci> [0..5]
[0,1,2,3,4,5]
```

So, we can write the above more simply

```haskell
quiz = [ 10 * i | i <- [0..5] ]
```

## QUIZ: Composing Comprehensions

What is the result of evaluating

```haskell
quiz = [(i,j) | i <- [0, 1]     -- a first selection
              , j <- [0, 1] ]   -- a second selection
```

**A.** Type error
**B.** `[]`
**C.** `[0,1]`
**D.** `[(0,0), (1,1)]`
**E.** `[(0,0), (0,1, (1,0), (1,1)]`

<br>
<br>
<br>
<br>
<br>  

## QUIZ: Composing Comprehensions

What is the result of evaluating

```haskell
quiz = [(i,j) | i <- [0, 1]
              , j <- [0, 1]
              , i == j      ]   -- condition!
```

**A.** Type error
**B.** `[]`
**C.** `[0,1]`
**D.** `[(0,0), (1,1)]`
**E.** `[(0,0), (0,1, (1,0), (1,1)]`

<br>
<br>
<br>
<br>
<br>
<br>

## shout revisited

How can we convert a string to upper-case, e.g.

```haskell
ghci> shout "like this"
"LIKE THIS"
```

Use comprehensions to write a *non-recursive" `shout`?

```haskell
shout :: String -> String
shout s = ???
```

<br>
<br>
<br>
<br>
<br>  

## QuickSort in Haskell


**Step 1: Write some tests**

```haskell
-- >>> sort []
-- ???

-- >>> sort [10]
-- ???

-- >>> sort [12, 1, 10]
-- ???
```

**Step 2: Write the type**


```haskell
sort :: ???
```

**Step 3: Write the code**


```haskell
sort []     = ???
sort (x:xs) = ???
```


(I) final

```haskell
sort :: [Int] -> [Int]
sort []     = []
sort (x:xs) = sort ls ++ [x] ++ sort rs
  where
    ls      = [ l | l <- xs, l <= x ]
    rs      = [ r | r <- xs, x <  r ]
```

<br>
<br>
<br>
<br>
<br>
<br>

### Haskell is purely functional

**Functional** = functions are *first-class values*

**Pure** = a program is an expression that evaluates to a value

  * no side effects!
  
  * unlike in Python, Java, etc:
  
    ```java
    public int f(int x) {
      calls++;                         // side effect: global variable update!
      System.out.println("calling f"); // side effect: writing to screen!
      launchMissile();                 // side effect: can't bring back home!
      return x * 2;
    }
    ```

  * in Haskell, a function of type `Int -> Int`
    Computes a *single integer output* from a *single integer input*
    Does **nothing else**
  
**Referential transparency:** The same expression always evaluates to the same value

<!-- 
  * More precisely: In a scope where `x1, ..., xn` are defined,
    all occurrences of `e` with `FV(e) = {x1, ..., xn}` have the same value
  -->

<br>
  
**Why is this good?**

  * easier to reason about (remember `x++` vs `++x` in C++?)
  * enables compiler optimizations 
  * especially great for parallelization (`e1 + e2`: we can always compute `e1` and `e2` in parallel!)
  
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ 

The function `head` returns the first element of a list.

What is the result of:

```haskell
goBabyGo :: Int -> [Int]
goBabyGo n = n : goBabyGo (n + 1)

quiz :: Int
quiz = head (goBabyGo 0)
```

**A.** Loops forever
**B.** Type error
**C.** `0`
**D.** `1`

## Haskell is Lazy

An expression is evaluated only when its result is needed!

```haskell
ghci> take 2 (goBabyGo 1)
[1,2]
```

Why?

<br>
<br>

```haskell
        take 2 (goBabyGo 1)
=>      take 2 (1 : goBabyGo 2)
=>      take 2 (1 : 2 : goBabyGo 3)
=> 1:   take 1 (    2 : goBabyGo 3)
=> 1:2: take 0 (        goBabyGo 3)
=> 1:2: []
```

<br>

**Why is this good?**

  * can implement cool stuff like infinite lists: `[1..]`
  
    ```haskell
    -- first n pairs of co-primes: 
    take n [(i,j) | i <- [1..],
                    j <- [1..i],
                    gcd i j == 1]
    ```

  
  * encourages simple, general solutions
  * but has its problems too :(
           
<br>
<br>
<br>
<br>
<br>

That's all folks!  
