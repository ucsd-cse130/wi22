---
title: Typeclasses
date: 2019-05-29
headerImg: books.jpg
---

## Past two Weeks

How to *implement* language constructs?

- Local variables and scope
- Environments and Closures
- Parsing

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

## Next two Weeks

Modern language features for structuring programs

- Overloading
- Type classes
- Monads

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


## Overloading Operators: Arithmetic

The `+` operator works for a bunch of different types.

For `Integer`:

```haskell
λ> 2 + 3
5
```

for `Double` precision floats:

```haskell
λ> 2.9 + 3.5
6.4
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

## Overloading Comparisons

Similarly we can _compare_ different types of values

```haskell
λ> 2 == 3
False

λ>  [2.9, 3.5] == [2.9, 3.5]
True

λ> ("cat", 10) < ("cat", 2)
False

λ> ("cat", 10) < ("cat", 20)
True
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



## Ad-Hoc Overloading

Seems unremarkable?

Languages since the dawn of time have supported "operator overloading"

- To support this kind of **ad--hoc polymorphism**

- Ad-hoc: "created or done for a particular purpose as necessary."

You really **need** to _add_ and _compare_ values of _multiple_ types!

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
<br>
<br>
<br>
<br>


## Haskell has no caste system

No distinction between operators and functions

- All are first class citizens!

But then, what type do we give to *functions* like `+` and `==` ?

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
<br>
<br>
<br>
<br>



## QUIZ

Which of the following would be appropriate types for `(+)` ?

**(A)** `(+) :: Integer -> Integer -> Integer`

**(B)** `(+) :: Double  -> Double  -> Double`

**(C)** `(+) :: a       -> a       -> a`

**(D)** _All_ of the above

**(E)** _None_ of the above

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

`Integer -> Integer -> Integer` is bad because?

- Then we cannot add `Double`s!

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

`Double -> Double -> Double` is bad because?

- Then we cannot add `Double`s!

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


`a -> a -> a` is bad because?

- That doesn't make sense, e.g. to add two `Bool` or two `[Int]` or two functions!

## Type Classes for Ad Hoc Polymorphism 

Haskell solves this problem with an *insanely slick*
mechanism called **type-classes**, introduced by [Wadler and Blott](http://portal.acm.org/citation.cfm?id=75283) 

![](/static/img/blott-wadler.png){#fig:types .align-center width=60%}

**BTW:** The paper is one of the clearest examples of academic writing I have seen.

The next time you hear a curmudgeon say all the best CS was done in the 60s,
just point them to the above.

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

## Qualified Types

To see the right type, lets ask:

```haskell
λ> :type (+)
(+) :: (Num a) => a -> a -> a
```

We call the above a **qualified type**. Read it as `+`

- takes in two `a` values and returns an `a` value

for any type `a` that 

- _is a_ `Num` or
- _implements_ the `Num` interface or
- _is an instance of_ a `Num`.

The name `Num` can be thought of as a _predicate_  or _constraint_ over types 

- Similar but different than [Java interfaces](https://www.parsonsmatt.org/2017/01/07/how_do_type_classes_differ_from_interfaces.html)

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



## Some types _are_ `Num`s 

Examples include `Integer`, `Double` etc

- Any such values of those types can be passed to `+`. 

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

## Other types _are not_ `Num`s 

Examples include `Char`, `String`, functions etc, 

- Values of those types _cannot_ be passed to `+`.

```haskell
λ> True + False

<interactive>:15:6:
    No instance for (Num Bool) arising from a use of ‘+’
    In the expression: True + False
    In an equation for ‘it’: it = True + False
```

**Aha!** _Now_ those `no instance for` error messages should make sense!

- Haskell is complaining that `True` and `False` are of type `Bool` 
- and that `Bool` is _not_ an instance of `Num`.

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


## Type Class is a Set of Operations

A type class is a collection of operations (functions) 
that must exist for the underlying type.

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


## The `Eq` Type Class

The simplest type class is perhaps, `Eq`

```haskell
class  Eq a  where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

A type `a` is _an instance of_ `Eq` if there are two functions

- `==` and `/=`

That determine if two `a` values are respectively _equal_ or _disequal_.

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

## The `Show` Type Class

The type class `Show` requires  that instances be convertible to `String`
(which can then be printed out)

```haskell
class  Show a  where
  show :: a -> String
```

Indeed, we can test this on different (built-in) types

```haskell
λ> show 2
"2"

λ> show 3.14
"3.14"

λ> show (1, "two", ([],[],[]))
"(1,\"two\",([],[],[]))"
```

(Hey, whats up with the funny `\"`?)

When we type an expression into ghci, it computes the value
and then calls `show` on the result. Thus, if we create a
*new* type by

```haskell
data Unshowable = A | B | C
```

and then create values of the type,

```haskell
λ> let x = A
λ> :type x
x :: Unshowable
```

but then we **cannot view** them

```haskell
λ> x

<interactive>:1:0:
    No instance for (Show Unshowable)
      arising from a use of `print' at <interactive>:1:0
    Possible fix: add an instance declaration for (Show Unshowable)
    In a stmt of a 'do' expression: print it
```

and we **cannot compare** them!

```haskell
λ> x == x

<interactive>:1:0:
    No instance for (Eq Unshowable)
      arising from a use of `==' at <interactive>:1:0-5
    Possible fix: add an instance declaration for (Eq Unshowable)
    In the expression: x == x
    In the definition of `it': it = x == x
```

Again, the previously incomprehensible type error message should
make sense to you.

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

## Creating Instances

Tell Haskell how to show or compare values of type `Unshowable`

By **creating instances** of `Eq` and `Show` for that type:

```haskell
instance Eq Unshowable where
  (==) A A = True           -- True if both inputs are A
  (==) B B = True           -- ...or B
  (==) C C = True           -- .. or C
  (==) _ _ = False          -- otherwise

  (/=) x y = not (x == y)   -- Test if `x == y` and negate result!
```

**EXERCISE** Lets *create* an `instance` for `Show Unshowable`

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

## Automatic Derivation

This is silly: we _should_ be able to compare and view `Unshowble` "automatically"!

Haskell lets us _automatically derive_ functions for some classes in the standard library.

To do so, we simply dress up the data type definition with

```haskell
data Showable = A' | B' | C'
  deriving (Eq, Show) -- tells Haskell to automatically generate instances
```

Now we have

```haskell
λ> let x' = A'

λ> :type x'
x' :: Showable

λ> x'
A'

λ> x' == x'
True

λ> x' == B'
False
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

## Standard Typeclass Hierarchy

Let us now peruse the definition of the `Num` typeclass.

```haskell
λ> :info Num
class (Eq a, Show a) => Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

A type `a` _is an instance of_ (i.e. _implements_) `Num` if

1. The type is *also* an instance of `Eq` and `Show`, and
2. There are functions for adding, multiplying, subtracting, negating
   etc values of that type.

In other words in addition to the "arithmetic" operations, we can
compare two `Num` values and we can view them (as a `String`.)

Haskell comes equipped with a rich set of built-in classes.

![Standard Typeclass Hierarchy](/static/img/haskell98-classes.png)

In the above picture, there is an edge from `Eq` and `Show` to `Num`
because for something to be a `Num` it must also be an `Eq` and `Show`.

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

## The `Ord` Typeclass

Another typeclass you've used already is the one for `Ord`ering values:

```haskell
λ> :info (<)
class Eq a => Ord a where
  ...
  (<) :: a -> a -> Bool
  ...
```

For example:

```haskell
λ> 2 < 3
True

λ> "cat" < "dog"
True
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

## QUIZ 

Recall the datatype:

```haskell
data Showable = A' | B' | C' deriving (Eq, Show)
```

What is the result of:

```haskell
λ> A' < B'
```

**(A)** `True`
**(B)** `False`
**(C)** Type error
**(D)** Run-time exception

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

## Using Typeclasses

Typeclasses integrate with the rest of Haskell's type system.

Lets build a small library for *Environments* mapping keys `k` to values `v`

```haskell
data Env k v
  = Def  v              -- default value `v` to be used for "missing" keys
  | Bind k v (Env k v)  -- bind key `k` to the value `v`
  deriving (Show)
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

## An API for `Env`

Lets write a small API for `Env`

```haskell
-- >>> let env0 = add "cat" 10.0 (add "dog" 20.0 (Def 0))

-- >>> get "cat" env0
-- 10

-- >>> get "dog" env0
-- 20

-- >>> get "horse" env0
-- 0
```

Ok, lets implement!

```haskell
-- | 'add key val env' returns a new env that additionally maps `key` to `val`
add :: k -> v -> Env k v -> Env k v
add key val env = ???

-- | 'get key env' returns the value of `key` and the "default" if no value is found

get :: k -> Env k v -> v
get key env = ???
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

Oops, y u no check?

## Constraint Propagation

Lets _delete_ the types of `add` and `get` and see what Haskell says their types are! 

```haskell
λ> :type get
get :: (Eq k) => k -> v -> Env k v -> Env k v
```

Haskell tells us that we can use any `k` value as a *key*
as long as the value is an instance of the `Eq` typeclass.

How, did GHC figure this out? 

- If you look at the code for `get` you'll see that we check if two keys _are equal_!

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

## EXERCISE (Do at home)

Write an *optimized* version of

- `add` that ensures the keys are in _increasing_ order,
- `get` that gives up and returns the "default" the moment
   we see a key thats larger than the one we're looking for.

_(How) do you need to change the type of `Env`?_

_(How) do you need to change the types of `get` and `add`?_

<br>
<br>
<br>
<br>
<br>
<br>

## Explicit Signatures

While Haskell is pretty good about inferring types in general, there are
cases when the use of type classes requires explicit annotations (which
change the behavior of the code.)

For example, `Read` is a built-in typeclass, where any instance `a` of
`Read` has a function

```haskell
read :: (Read a) => String -> a
```

which can parse a string and turn it into an `a`.

That is, `Read` is the _opposite_ of `Show`.

## Quiz

What does the expression `read "2"` evaluate to?

**(A)** compile time error

**(B)** `"2" :: String`

**(C)** `2   :: Integer`

**(D)** `2.0 :: Double`

**(E)** run-time exception

<br>
<br>
<br>
<br>
<br>
<br>

Haskell is foxed!

- Doesn't know _what type_ to convert the string to!
- Doesn't know _which_ of the `read` functions to run!

Did we want an `Int` or a `Double` or maybe something else altogether?

Thus, here an **explicit type annotation** is needed to tell Haskell
what to convert the string to: 

```haskell
λ> (read "2") :: Int
2

λ> (read "2") :: Float
2.0
```

Note the different results due to the different types.

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

## Creating Typeclasses

Typeclasses are useful for *many* different things.

We will see some of those over the next few lectures.

Lets conclude today's class with a quick example that provides a small taste. 

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

## JSON

*JavaScript Object Notation* or [JSON](http://www.json.org/) is a simple format for
transferring data around. Here is an example:

```json
{ "name"    : "Ranjit"
, "age"     : 41.0
, "likes"   : ["guacamole", "coffee", "bacon"]
, "hates"   : [ "waiting" , "grapefruit"]
, "lunches" : [ {"day" : "monday",    "loc" : "zanzibar"}
              , {"day" : "tuesday",   "loc" : "farmers market"}
              , {"day" : "wednesday", "loc" : "harekrishna"}
              , {"day" : "thursday",  "loc" : "faculty club"}
              , {"day" : "friday",    "loc" : "coffee cart"} ]
}
```

In brief, each JSON object is either

- a *base* value like a string, a number or a boolean,

- an (ordered) *array* of objects, or

- a set of *string-object* pairs.

## A JSON Datatype

We can represent (a subset of) JSON values with the Haskell datatype

```haskell
data JVal
  = JStr  String
  | JNum  Double
  | JBool Bool
  | JObj  [(String, JVal)]
  | JArr  [JVal]
  deriving (Eq, Ord, Show)
```

Thus, the above JSON value would be represented by the `JVal`

```haskell
js1 =
  JObj [("name", JStr "Ranjit")
       ,("age",  JNum 41.0)
       ,("likes",   JArr [ JStr "guacamole", JStr "coffee", JStr "bacon"])
       ,("hates",   JArr [ JStr "waiting"  , JStr "grapefruit"])
       ,("lunches", JArr [ JObj [("day",  JStr "monday")
                                ,("loc",  JStr "zanzibar")]
                         , JObj [("day",  JStr "tuesday")
                                ,("loc",  JStr "farmers market")]
                         , JObj [("day",  JStr "wednesday")
                                ,("loc",  JStr "hare krishna")]
                         , JObj [("day",  JStr "thursday")
                                ,("loc",  JStr "faculty club")]
                         , JObj [("day",  JStr "friday")
                                ,("loc",  JStr "coffee cart")]
                         ])
       ]
```

## Serializing Haskell Values to JSON

Lets write a small library to _serialize_ Haskell values as JSON. 

We could write a bunch of functions like

```haskell
doubleToJSON :: Double -> JVal
doubleToJSON = JNum

stringToJSON :: String -> JVal
stringToJSON = JStr

boolToJSON   :: Bool -> JVal
boolToJSON   = JBool
```

## Serializing Collections

But what about collections, namely _lists_ of things?

```haskell
doublesToJSON    :: [Double] -> JVal
doublesToJSON xs = JArr (map doubleToJSON xs)

boolsToJSON      :: [Bool] -> JVal
boolsToJSON xs   = JArr (map boolToJSON xs)

stringsToJSON    :: [String] -> JVal
stringsToJSON xs = JArr (map stringToJSON xs)
```

This is **getting rather tedious**

- We are rewriting the same code :(

<br>
<br>
<br>
<br>
<br>
<br>

## Serializing Collections (refactored with HOFs)

You could abstract by making the *individual-element-converter* a parameter

```haskell
xsToJSON :: (a -> JVal) -> [a] -> JVal
xsToJSON f xs = JArr (map f xs)

xysToJSON :: (a -> JVal) -> [(String, a)] -> JVal
xysToJSON f kvs = JObj [ (k, f v) | (k, v) <- kvs ]
```

But this is *still rather tedious** as you have to pass 
in the individual data converter (yuck)

```haskell
λ> doubleToJSON 4
JNum 4.0

λ> xsToJSON stringToJSON ["coffee", "guacamole", "bacon"]
JArr [JStr "coffee",JStr "guacamole",JStr "bacon"]

λ> xysToJSON stringToJSON [("day", "monday"), ("loc", "zanzibar")]
JObj [("day",JStr "monday"),("loc",JStr "zanzibar")]
```

This gets more hideous when you have richer objects like

```haskell
lunches = [ [("day", "monday"),    ("loc", "zanzibar")]
          , [("day", "tuesday"),   ("loc", "farmers market")]
          ]
```

because we have to go through gymnastics like

```haskell
λ> xsToJSON (xysToJSON stringToJSON) lunches
JArr [ JObj [("day",JStr "monday")   ,("loc",JStr "zanzibar")]
     , JObj [("day",JStr "tuesday")  ,("loc",JStr "farmers market")]
     ]
```

Yikes. So much for _readability_

Is it too much to ask for a magical `toJSON` that _just works?_

## Typeclasses To The Rescue

Lets _define_ a typeclass that describes types `a` that can be converted to JSON.

```haskell
class JSON a where
  toJSON :: a -> JVal
```

Now, just make all the above instances of `JSON` like so

```haskell
instance JSON Double where
  toJSON = JNum

instance JSON Bool where
  toJSON = JBool

instance JSON String where
  toJSON = JStr
```

This lets us uniformly write

```haskell
λ> toJSON 4
JNum 4.0

λ> toJSON True
JBool True

λ> toJSON "guacamole"
JStr "guacamole"
```

## Bootstrapping Instances

The real fun begins when we get Haskell to automatically
bootstrap the above functions to work for lists and key-value lists!

```haskell
instance JSON a => JSON [a] where
  toJSON xs = JArr (map toJSON xs)
```

The above says, if `a` is an instance of `JSON`, that is,
if you can convert `a` to `JVal` then here's a generic
recipe to convert lists of `a` values!

```haskell
λ> toJSON [True, False, True]
JArr [JBln True, JBln False, JBln True]

λ> toJSON ["cat", "dog", "Mouse"]
JArr [JStr "cat", JStr "dog", JStr "Mouse"]
```

or even lists-of-lists!

```haskell
λ> toJSON [["cat", "dog"], ["mouse", "rabbit"]]
JArr [JArr [JStr "cat",JStr "dog"],JArr [JStr "mouse",JStr "rabbit"]]
```

We can pull the same trick with key-value lists

```haskell
instance (JSON a) => JSON [(String, a)] where
  toJSON kvs = JObj [ (k, toJSON v) | (k, v) <- kvs ]
```

after which, we are all set!

```haskell
λ> toJSON lunches
JArr [ JObj [ ("day",JStr "monday"), ("loc",JStr "zanzibar")]
     , JObj [("day",JStr "tuesday"), ("loc",JStr "farmers market")]
     ]
```

It is also useful to bootstrap the serialization for tuples (up to some
fixed size) so we can easily write "non-uniform" JSON objects where keys
are bound to values with different shapes.

```haskell
instance (JSON a, JSON b) => JSON ((String, a), (String, b)) where
  toJSON ((k1, v1), (k2, v2)) =
    JObj [(k1, toJSON v1), (k2, toJSON v2)]

instance (JSON a, JSON b, JSON c) => JSON ((String, a), (String, b), (String, c)) where
  toJSON ((k1, v1), (k2, v2), (k3, v3)) =
    JObj [(k1, toJSON v1), (k2, toJSON v2), (k3, toJSON v3)]

instance (JSON a, JSON b, JSON c, JSON d) => JSON ((String, a), (String, b), (String, c), (String,d)) where
  toJSON ((k1, v1), (k2, v2), (k3, v3), (k4, v4)) =
    JObj [(k1, toJSON v1), (k2, toJSON v2), (k3, toJSON v3), (k4, toJSON v4)]

instance (JSON a, JSON b, JSON c, JSON d, JSON e) => JSON ((String, a), (String, b), (String, c), (String,d), (String, e)) where
  toJSON ((k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5)) =
    JObj [(k1, toJSON v1), (k2, toJSON v2), (k3, toJSON v3), (k4, toJSON v4), (k5, toJSON v5)]
``` 

Now, we can simply write

```haskell
hs = (("name"   , "Ranjit")
     ,("age"    , 41.0)
     ,("likes"  , ["guacamole", "coffee", "bacon"])
     ,("hates"  , ["waiting", "grapefruit"])
     ,("lunches", lunches)
     )
```

which is a Haskell value that describes our running JSON example, 
and can convert it directly like so

```haskell
js2 = toJSON hs
```

## EXERCISE: Serializing Environments

To wrap everything up, lets write a routine to serialize our `Env`

```haskell
instance JSON (Env k v) where
  toJSON env = ???
```

and presto! our serializer *just works*

```haskell
λ> env0
Bind "cat" 10.0 (Bind "dog" 20.0 (Def 0))

λ> toJSON env0
JObj [ ("cat", JNum 10.0)
     , ("dog", JNum 20.0)
     , ("def", JNum 0.0)
     ]
```

Thats it for today.

We will see much more type class awesomeness in the next few lectures...
