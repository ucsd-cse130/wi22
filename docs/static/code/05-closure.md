---
title: Closures
date: 2019-05-15
headerImg: books.jpg
---

## Past three weeks

How to *use* essential language constructs?

- Data Types
- Recursion
- Higher-Order Functions

## Next two weeks

How to *implement* language constructs?

- Local variables and scope
- Environments and Closures

<!--
- Type Inference
-->

### Interpreter

How do we *represent* and *evaluate* a program?

<!-- 
- How do we *prove properties* about our interpreter
  (e.g. that certain programs never crash)?  
  -->

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

How should we evaluate this expression?


```haskell
   eval []
    {let c = 42 in let cTimes = \x -> c * x in cTimes 2}
=> eval [c:42] 
                  {let cTimes = \x -> c * x in cTimes 2}
=> eval [cTimes:???, c:42] 
                                              {cTimes 2}
```

<br>


What is the **value** of `cTimes`???

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Rethinking our values

**Until now:** a program *evaluates* to an integer (or fails)

```haskell
type Value = Int

type Env = [(Id, Value)]

eval :: Env -> Expr -> Value
```

<br>
<br>

What do these programs evaluate to?

```haskell
(1)
\x -> 2 * x
==> ???

(2)
let f = \x -> \y -> 2 * (x + y) in
f 5
==> ???
```

(I) final

    Conceptually, (1) evaluates to itself (not exactly, see later).
    while (2) evaluates to something equivalent to `\y -> 2 * (5 + y)`
    

<br>
<br>
<br>
<br>
<br>
<br>

**Now:** a program evaluates to an integer or *a lambda abstraction* (or fails)

  - Remember: functions are *first-class* values
  
<br>

Let's change our definition of values!  

```haskell
data Value = VNum Int
           | VLam ??? -- What info do we need to store?

-- Other types stay the same
type Env = [(Id, Value)]

eval :: Env -> Expr -> Value
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

## Function values

How should we represent a function value?

```haskell
let c = 42 in
let cTimes = \x -> c * x in 
cTimes 2
```

We need to store enough information about `cTimes`
so that we can later evaluate any *application* of `cTimes`
(like `cTimes 2`)!

<br>
<br>

First attempt:

```haskell
data Value = VNum Int
           | VLam Id Expr -- formal + body
```

<br>
<br>

Let's try this!

```haskell
   eval []         
    {let c = 42 in let cTimes = \x -> c * x in cTimes 2}
=> eval [c:42] 
                  {let cTimes = \x -> c * x in cTimes 2}
=> eval [cTimes:(\x -> c*x), c:42] 
                                              {cTimes 2}
    -- evaluate the function:
=> eval [cTimes:(\x -> c*x), c:42]
                                       {(\x -> c * x) 2} 
    -- evaluate the argument, bind to x, evaluate body:
=> eval [x:2, cTimes:(\x -> c*x), c:42] 
                                              {c * x}
=>                                            42 * 2
=>                                            84
```

<br>

Looks good... can you spot a problem?

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

What should this evaluate to?

```haskell
let c = 42 in
let cTimes = \x -> c * x in -- but which c???
let c = 5 in
cTimes 2
```

**(A)** `84`

**(B)** `10`

**(C)** Error: multiple definitions of `c`

<br>

(I) final

    *Answer:* A

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Static vs Dynamic Scoping

What we want:

```haskell
let c = 42 in
let cTimes = \x -> c * x in
let c = 5 in
cTimes 2

=> 84
```

**Lexical** (or **static**) scoping:

  - each occurrence of a variable refers to the most recent binding *in the program text*
  - definition of each variable is unique and known *statically*
  - good for readability and debugging: don’t have to figure out where a variable got "assigned"

  
<br>
<br>

What we **don't** want:

```haskell
let c = 42 in
let cTimes = \x -> c * x in
let c = 5 in
cTimes 2

=> 10
```

**Dynamic** scoping:

  - each occurrence of a variable refers to the most recent binding *during program execution*
  - can't tell where a variable is defined just by looking at the function body
  - nightmare for readability and debugging:
    
```haskell
let cTimes = \x -> c * x in
let c = 5 in
let res1 = cTimes 2 in -- ==> 10
let c = 10 in
let res2 = cTimes 2 in -- ==> 20!!!
res2 - res1
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

## Function values

```haskell
data Value = VNum Int
           | VLam Id Expr -- formal + body
```

This representation can only implement dynamic scoping!

```haskell
let c = 42 in
let cTimes = \x -> c * x in
let c = 5 in
cTimes 2
```

evaluates as:

```haskell
   eval []         
   {let c = 42 in let cTimes = \x -> c * x in let c = 5 in cTimes 2}
=> eval [c:42] 
                 {let cTimes = \x -> c * x in let c = 5 in cTimes 2}
=> eval [cTimes:(\x -> c*x), c:42] 
                                             {let c = 5 in cTimes 2}
=> eval [c:5, cTimes:(\x -> c*x), c:42] 
                                                          {cTimes 2}
=> eval [c:5, cTimes:(\x -> c*x), c:42]
                                                   {(\x -> c * x) 2} 
=> eval [x:2, c:5, cTimes:(\x -> c*x), c:42] 
                                                          {c * x}
  -- latest binding for c is 5!
=>                                                         5 * 2
=>                                                         10
```

**Lesson learned:** need to remember what `c` was bound to when `cTimes` was defined!

  - i.e. "freeze" the environment at function definition

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Closures

To implement lexical scoping, we will represent function values as *closures*

<br>

**Closure** = *lambda abstraction* (formal + body) + *environment* at function definition 

<br>
              
```haskell
data Value = VNum Int
           | VClos Env Id Expr -- env + formal + body
``` 

<br>

Our example:

```haskell
   eval []         
   {let c = 42 in let cTimes = \x -> c * x in let c = 5 in cTimes 2}
=> eval [c:42] 
                 {let cTimes = \x -> c * x in let c = 5 in cTimes 2}
   -- remember current env:
=> eval [cTimes:<[c:42], \x -> c*x>, c:42] 
                                             {let c = 5 in cTimes 2}
=> eval [c:5, cTimes:<[c:42], \x -> c*x>, c:42] 
                                                          {cTimes 2}
=> eval [c:5, cTimes:<[c:42], \x -> c*x>, c:42]
                                           {<[c:42], \x -> c * x> 2}
  -- restore env to the one inside the closure, then bind 2 to x:                                                 
=> eval [x:2, c:42] 
                                                          {c * x}
=>                                                        42 * 2
=>                                                        84
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

Which variables should be saved in the closure environment of `f`?

```haskell
let a = 20 in
let f = 
  \x -> let y = x + 1 in
        let g = \z -> y + z in
        a + g x 
  in ...        
```

**(A)** `a`

**(B)** `a x`

**(C)** `y g`

**(D)** `a y g`

**(E)** `a x y g z`


<br>

(I) final

    *Answer:* A

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Free vs bound variables

- An occurrence of `x` is **free** if it is not **bound**
- An occurrence of `x` is **bound** if it's inside 
    - `e2` where `let x = e1 in e2`
    - `e` where `\x -> e`
- A closure environment has to save *all free variables* of a function definition!


```haskell
let a = 20 in
let f = 
  \x -> let y = x + 1 in
        let g = \z -> y + z in
        a + g x -- a is the only free variable!
  in ...        
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

## Evaluator

Let's modify our evaluator to handle functions!

```haskell
data Value = VNum Int
           | VClos Env Id Expr -- env + formal + body
           
eval :: Env -> Expr -> Value
eval env (Num n)        = VNum n -- must wrap in VNum now!
eval env (Var x)        = lookup x env
eval env (Bin op e1 e2) = VNum (f v1 v2)
  where
    (VNum v1) = eval env e1
    (VNum v2) = eval env e2
    f = ... -- as before
eval env (Let x e1 e2) = eval env' e2
  where
    v = eval env e1
    env' = add x v env
eval env (Lam x body) = ??? -- construct a closure
eval env (App fun arg) = ??? -- eval fun, then arg, then apply
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

Evaluating functions:

* **Construct a closure**: save environment at function definition
* **Apply a closure**: restore saved environment, add formal, evaluate the body

```haskell
eval :: Env -> Expr -> Value
...
eval env (Lam x body) = VClos env x body
eval env (App fun arg) = eval bodyEnv body
  where
    (VClos closEnv x body) = eval env fun -- eval function to closure
    vArg                   = eval env arg -- eval argument
    bodyEnv                = add x vArg closEnv
```


## QUIZ

With `eval` as defined above, what does this evaluate to?

```haskell
let f = \x -> x + y in
let y = 10 in
f 5
```

**(A)** `15`

**(B)** `5`

**(C)** Error: unbound variable `x`

**(D)** Error: unbound variable `y`

**(E)** Error: unbound variable `f`


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

```haskell
   eval []         
     {let f = \x -> x + y in let y = 10 in f 5}
=> eval [f:<[], \x -> x + y>]
                            {let y = 10 in f 5}
=> eval [y:10, f:<[], \x -> x + y>]
                                          {f 5}
=> eval [y:10, f:<[], \x -> x + y>]
                          {<[], \x -> x + y> 5}  
=> eval [x:5] -- env got replaced by closure env + formal!
                                     {x + y}  -- y is unbound!
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>        

## QUIZ

With `eval` as defined above, what does this evaluate to?

```haskell
let f = \n -> n * f (n - 1) in
f 5
```

**(A)** `120`

**(B)** Evaluation does not terminate

**(C)** Error: unbound variable `f`

<br>

(I) final

    *Answer:* C

<br>
<br>
<br>
<br>
<br>
<br>
<br>

```haskell
   eval []         
       {let f = \n -> n * f (n - 1) in f 5}
=> eval [f:<[], \n -> n * f (n - 1)>]
                                      {f 5}
=> eval [f:<[], \n -> n * f (n - 1)>]
              {<[], \n -> n * f (n - 1)> 5}   
=> eval [n:5] -- env got replaced by closure env + formal!
                         {n * f (n - 1)} -- f is unbound!
```


**Lesson learned:** to support recursion, 
we need a different way of constructing the closure environment!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The Nano Language

Features of Nano:

1. Arithmetic expressions **[done]**
2. Variables and let-bindings **[done]**
3. Functions **[done]**
4. Recursion **[this is part of HW4]**


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

<!-- 
## Formalizing Nano

**Goal:** we want to guarantee properties about programs, such as:

  * evaluation is deterministic
  * all programs terminate
  * certain programs never fail at run time
  * etc.
  
To prove theorems about programs we first need to define formally
  
  * their *syntax* (what programs look like)
  * their *semantics* (what it means to run a program)
  
Let's start with Nano1 (Nano w/o functions) and prove some stuff!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Nano1: Syntax

We need to define the syntax for *expressions* (*terms*)
and *values* using a grammar:

```haskell
e ::= n | x             -- expressions
    | e1 + e2
    | let x = e1 in e2

v ::= n                 -- values
```

where $n \in \mathbb{N}, x \in \mathrm{Var}$

<br>
<br>
<br>

## Nano1: Operational Semantics

**Operational semantics** defines how to execute a program step by step

<br>

Let's define a *step relation* (*reduction relation*) `e => e'`

  * "expression `e` makes a step (reduces in one step) to an expression `e'`
  
<br>
<br>
  
We define the step relation *inductively* through a set of *rules*:

```haskell
               e1 => e1'        -- premise
[Add-L]   --------------------
          e1 + e2 => e1' + e2   -- conclusion

              e2 => e2'
[Add-R]   --------------------
          n1 + e2 => n1 + e2'
          
[Add]     n1 + n2 => n       where n == n1 + n2          

                        e1 => e1'
[Let-Def] --------------------------------------
          let x = e1 in e2 => let x = e1' in e2
        
[Let]     let x = v in e2 => e2[x := v]
```

Here `e[x := v]` is a value substitution:

```haskell
x[x := v]                  = v
y[x := v]                  = y            -- assuming x /= y
n[x := v]                  = n
(e1 + e2)[x := v]          = e1[x := v] + e2[x := v]
(let x = e1 in e2)[x := v] = let x = e1[x := v] in e2
(let y = e1 in e2)[x := v] = let y = e1[x := v] in e2[x := v]
```

Do not have to worry about capture, because `v` is a value (has not free variables!)


<br>
<br>

A reduction is *valid* if we can build its **derivation** by "stacking" the rules:

```haskell
    [Var] --------------------
              1 + 2 => 3
[Add-L] -----------------------
        (1 + 2) + 5  =>  3 + 5
```

<br>
<br>


Do we have rules for all kinds of expressions?

<br>
<br>
<br>
<br>
<br>

### 1. Normal forms

There are no reduction rules for:

  * `n`
  * `x`
  
Both of these expressions are *normal forms* (cannot be further reduced), however:

  * `n` is a *value*
      * intuitively, corresponds to successful evaluation
  * `x` is *not* a value
      * intuitively, corresponds to a run-time error!
      * we say the program `x` is **stuck**

<br>
<br>
<br>
<br>  
<br>

### 2. Evaluation order

In `e1 + e2`, which side should we evaluate first?

In other words, which one of these reductions is valid (or both)?

  (1) `(1 + 2) + (4 + 5)  =>  3 + (4 + 5)`
  (2) `(1 + 2) + (4 + 5)  =>  (1 + 2) + 9`
  
<br>
<br>

Reduction (1) is *valid* because we can build a **derivation** using the rules:
  
```haskell
          [Add] ----------
                1 + 2 => 3
[Add-L] ----------------------------------
        (1 + 2) + (4 + 5)  =>  3 + (4 + 5)
```

Reduction (2) is *invalid* because we cannot build a derivation:

  * there is *no rule* whose conclusion matches this reduction!

```haskell
                    ??? 
[???] -----------------------------------
      (1 + 2) + (4 + 5)  =>  (1 + 2) + 9
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ

```haskell
                        e1 => e1'
[Let-Def] --------------------------------------
          let x = e1 in e2 => let x = e1' in e2
        
[Let]     let x = v in e2 => e2[x := v]
```

If these are the only rules for `let` bindings,
which reductions are valid?

**(A)** `(let x = 1 + 2 in 4 + 5 + x)  =>  (let x = 3 in 4 + 5 + x)`

**(B)** `(let x = 1 + 2 in 4 + 5 + x)  =>  (let x = 1 + 2 in 9 + x)`

**(C)** `(let x = 1 + 2 in 4 + 5 + x)  =>  (4 + 5 + 1 + 2)`

**(D)** A and B

**(E)** All of the above


<br>

(I) final

    *Answer:* A

<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Evaluation relation

Like in $\lambda$-calculus, we define the **multi-step reduction** relation `e =*> e'`:

`e =*> e'` iff there exists a sequence of expressions `e1, ..., en` such that

  * `e = e1`
  * `en = e'`
  * `ei => e(i+1)` for each `i in [0..n)`
  
<br>

*Example:*

```haskell
    (1 + 2) + (4 + 5)  
=*> 3 + 9
```

because
```haskell
   (1 + 2) + (4 + 5)  
=> 3       + (4 + 5)
=> 3       + 9
```

<br>
<br>

Now we define the **evaluation relation** `e =~> e'`:

`e =~> e'` iff

  * `e =*> e'`
  * `e'` is in normal form

<br>

Example:

```haskell
    (1 + 2) + (4 + 5)  
=~> 12
```

because

```haskell
   (1 + 2) + (4 + 5)  
=> 3       + (4 + 5)
=> 3       + 9
=> 12
```

and `12` is a *value* (normal form)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Theorems about Nano1

Let's prove something about Nano1!

  1. Every Nano1 program terminates
  2. Closed Nano1 programs don't get stuck  
  3. *Corollary (1 + 2):* Every closed Nano1 program evaluates to a value
  
<br>

How do we prove theorems about languages?

**By induction.**


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Mathematical induction in PL

### 1. Induction on natural numbers

To prove $\forall n . P(n)$ we need to prove:

  * *Base case:* $P(0)$
  * *Inductive case:* $P(n + 1)$ assuming the *induction hypothesis* (IH): that $P(n)$ holds
  
<br>  
  
Compare with inductive definition for natural numbers:

```haskell
data Nat = Zero     -- base case
         | Succ Nat -- inductive case
```

No reason why this would only work for natural numbers...

In fact we can do induction on *any* inductively defined mathematical object (= any datatype)!

  * lists
  * trees
  * programs (terms)
  * etc
  
<br>
<br>
<br>

### 2. Induction on terms

```haskell
e ::= n | x
    | e1 + e2
    | let x = e1 in e2
```

To prove $\forall e . P(e)$ we need to prove:

  * *Base case 1:* `P(n)`
  * *Base case 2:* `P(x)`
  * *Inductive case 1:* `P(e1 + e2)` assuming the IH: that `P(e1)` and `P(e2)` hold
  * *Inductive case 2:* `P(let x = e1 in e2)` assuming the IH: that `P(e1)` and `P(e2)` hold
  
<br>
<br>
<br>

### 3. Induction on derivations

Our reduction relation `=>` is also defined *inductively*!

  * Axioms are bases cases
  * Rules with premises are inductive cases

To prove $\forall e,e' . P(e \Rightarrow e')$ we need to prove:

  * *Base cases:* `[Add]`, `[Let]`
  * *Inductive cases:* `[Add-L]`, `[Add-R]`, `[Let-Def]` assuming the IH: that `P` holds of their premise
  
  
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Theorem: Termination

**Theorem I** [Termination]: For any expression `e` there exists `e'` such that `e =~> e'`.

Proof idea: let's define the *size* of an expression such that

  * size of each expression is positive
  * each reduction step strictly decreases the size
  
Then the length of the execution sequence for `e` is *bounded* by the size of `e`!

<br>

```haskell
size n                  = ???
size x                  = ???
size (e1 + e1)          = ???
size (let x = e1 in e2) = ???
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Term size:

```haskell
size n                  = 1
size x                  = 1
size (e1 + e1)          = size e1 + size e2
size (let x = e1 in e2) = size e1 + size e2
```

**Lemma 1**: For any `e`, `size e > 0`.

**Proof:** By induction on the *term* `e`.

  * *Base case 1:* `size n = 1 > 0`
  * *Base case 2:* `size x = 1 > 0`
  * *Inductive case 1:* `size (e1 + e2) = size e1 + size e2 > 0` because `size e1 > 0` and `size e2 > 0` by IH.
  * *Inductive case 2:* similar.


**QED.**

<br>
<br>
<br>
<br>

**Lemma 2**: For any `e, e'` such that `e => e'`, `size e' < size e`.
  
**Proof:** By induction on the *derivation* of `e => e'`.

*Base case* `[Add]`.

  * Given: the root of the derivation is `[Add]`: `n1 + n2 => n` where `n = n1 + n2`
  * To prove: `size n < size (n1 + n2)` 
  * `size n = 1 < 2 = size (n1 + n2)`

*Inductive case* `[Add-L]`.

  * Given: the root of the derivation is `[Add-L]`:
  
```haskell  
     e1 => e1'
--------------------------
e1 + e2 => e1' + e2
```

  * To prove: `size (e1' + e2) < size (e1 + e2)`
  * IH: `size e1' < size e1`
  
```
  size (e1' + e2) 
= -- def. size
  size e1' + size e2 
< -- IH
  size e1 + size e2
= -- def. size
  size (e1 + e2)
```
  
*Inductive case* `[Add-R]`. Try at home   
  
*Base case* `[Let]`.

  * Given: the root of the derivation is `[Let]`: `let x = v in e2 => e2[x := v]`
  * To prove: `size (e2[x := v]) < size (let x = v in e2)` 
  
```
  size (e2[x := v]) 
= -- auxiliary lemma!
  size e2 
< -- IH
  size v + size e2
= -- def. size
  size (let x = v in e2)
```  

*Inductive case* `[Let-Def]`. Try at home 

**QED.**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ

```haskell
                        e1 => e1'
[Let-Def] --------------------------------------
          let x = e1 in e2 => let x = e1' in e2
```

What is the IH for the inductive case `[Let-Def]`?

**(A)** `e1 => e1'`

**(B)** `size e1' < size e1`

**(C)** `size (let x = e1 in e2) < size (let x = e1' in e2)`

<br>

(I) final

    *Answer:* B

<br>
<br>
<br>
<br>
<br>
<br>
<br>



## Theorem: Closed Programs don't Get Stuck

First we need to define what are free variables `fv` of an expression:

```haskell
fv n                  = {}
fv x                  = {x}
fv (e1 + e1)          = fv e1 + fv e2
fv (let x = e1 in e2) = fv e1 + fv e2 / {x}
```

**Theorem II** [No errors]: For any `e` such that `fv e = {}`, 
if `e =~> e'` then `e'` is a value.

Proof idea:

  * Proof by induction on the number of reduction steps
  * At the beginning we are not stuck
  * With $k + 1$ steps: after the first step, the expression is still closed, so the other $k$ steps follow by IH


**Proof:** by induction on the number $k$ of reduction steps:

  * Base case ($k = 0$): `e` is not stuck.
    We need an auxiliary lemma that a closed expression cannot be stuck (Lemma 3)
  * Inductive case ($k > 0$): then `e => e1 =~> e'`. 
    To apply IH, we need to show: `fv e1 = {}` (Lemma 4).    
    
<br>
<br>
<br>

**Lemma 3**: For any `e` such that `fv e = {}`, 
either `e` is a value or there exists `e'` such that `e => e'`

**Proof:** By induction on the *term* `e `.

  * *Base case* `n`: It's a value.
  * *Base case* `x`: `fv x = {x} /= {}`: contradiction!
  * *Inductive case* `e1 + e2`: Since `fv (e1 + e2) = {}`, 
    we know that `fv e1 = {}` and `fv e2 = {}`.
    So we can apply IH to conclude that `e1` (resp. `e2`) is a value or steps to some `e1'` (resp. `e2'`).
    If `e1 => e1'`, then [Add-L] applies.
    Otherwise `e1` is a value; then if `e2 => e2'`, then [Add-R] applies.
    Otherwise `e2` is also a value, so [Add] applies.
  
  * *Inductive case* `let x = e1 in e2`: Since `fv (let x = e1 in e2) = {}`, 
    we know that `fv e1 = {}`.
    So we can apply IH to conclude that `e1` is a value or steps to some `e1'`.
    If `e1 => e1'`, then [Let-Def] applies.
    Otherwise `e1` is a value; then [Let] applies.
    

<br>
<br>
<br>

**Lemma 4**: For any `e` such that `fv e = {}`, 
if `e => e'` then `fv e' = {}`

**Proof:** By induction on the *derivation* of `e => e'`.
  
*Base case* `[Add]`. Try at home.

  * Given: the root of the derivation is `[Add]`: `n1 + n2 => n` where `n = n1 + n2`
  * To prove: `fv n = {}` (by definition of `fv`)

*Inductive case* `[Add-L]`.

  * Given: the root of the derivation is `[Add-L]`:
  
```haskell  
     e1 => e1'
--------------------------
e1 + e2 => e1' + e2
```

  * Given: `fv (e1 + e2) = {}`
  * To prove: `fv (e1' + e2) = {}'`
  * IH: if `fv e1 = {}` then `fv e1' = {}`
  
```haskell
  fv (e1 + e2) = {}
<==> -- def. fv
  (fv e1 = {}) & (fv e2 = {})
==> -- IH
  (fv e1' = {}) & (fv e2 = {})
<==> -- def fv
  fv (e1' + e2) = {}
```    
  
*Inductive case* `[Add-R]`. Try at home   

*Base case* `[Let]`.

  * Given: the root of the derivation is `[Let]`: `let x = n in e2 => e2[x := n]`
  * Given: `fv (let x = n in e2) = {}`
  * To prove: `fv (e2[x := n]) = {}`
    
```haskell
  fv (e2[x := n])
== -- auxiliary lemma!
  fv e2 / {x}
== -- def fv  
  fv (let x = n in e2)
== -- given
  {}
```    

*Inductive case* `[Let-Def]`. Try at home.

**QED.**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Nano2: adding functions

<br>
<br>

### Syntax

We need to extend the syntax of expressions and values:

```haskell
e ::= n | x             -- expressions
    | e1 + e2
    | let x = e1 in e2
    | \x -> e       -- abstraction
    | e1 e2         -- application

v ::= n                 -- values
    | \x -> e       -- abstraction
```

<br>
<br>

### Operational semantics

We need to extend our reduction relation with rules for abstraction and application:

```haskell
           e1 => e1'
[App-L] ----------------
        e1 e2 => e1' e2
        
          e => e'
[App-R] ------------
        v e => v e'        
        
[App]   (\x -> e) v => e[x := v]          
```

### QUIZ

With rules defined above, which reductions are valid?

**(A)** `(\x y -> x + y) 1 (1 + 2)  =>  (\x y -> x + y) 1 3`
 
**(B)** `(\x y -> x + y) 1 (1 + 2)  =>  (\y -> 1 + y) (1 + 2)`

**(C)** `(\y -> 1 + y) (1 + 2)  =>  (\y -> 1 + y) 3`

**(D)** `(\y -> 1 + y) (1 + 2)  =>  1 + 1 + 2`

**(E)** B and C
   
<br>

(I) final

    *Answer:* E

<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Evaluation Order

```haskell
   ((\x y -> x + y) 1) (1 + 2)
=> (\y -> 1 + y) (1 + 2)       -- [App-L], [App]
=> (\y -> 1 + y) 3             -- [App-R], [Add]
=> 1 + 3                       -- [App]
=> 4                           -- [Add]
```

Our rules define **call-by-value**:

  1. Evaluate the function (to a lambda)
  2. Evaluate the argument (to some value)
  3. "Make the call": make a substitution of formal to actual in the body of the lambda
  
The alternative is **call-by-name**:

  * do not evaluate the argument before "making the call"
  * can we modify the application rules for Nano2 to make it call-by-name?
  
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
  
  
## Theorems about Nano2

Let's prove something about Nano2!

  1. Every Nano2 program terminates (?)
  2. Closed Nano2 programs don't get stuck (?)
  
### QUIZ

Are these theorems still true?

**(A)** Both true

**(B)** 1 is true, 2 is false

**(C)** 1 is false, 2 is true

**(D)** Both false
  
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
  
  
## Theorems about Nano2

  1. Every Nano2 program terminates (?)
  
     What about `(\x -> x x) (\x -> x x)`?
  
  2. Closed Nano2 programs don't get stuck (?)
  
     What about `1 2`?
     
Both theorems are now false!

To recover these properties, we need to add *types*:

  1. Every *well-typed* Nano2 program terminates
    
  2. *Well-typed* Nano2 programs don't get stuck

We'll do that next week!  
     
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

That's all folks!


-->
