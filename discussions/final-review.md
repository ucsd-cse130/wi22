---
title: Final Exam Review
headerImg: sea.jpg
date: 2020-03-17
---
## Tips for Writing Programs about Programming Languages

### Start with the base cases

They are usually more straightforward and don't require thinking about recursion.

### Pay attention to the types you have and what you're trying to produce

My process for writing `eval env (EVar x)` looks something like:

1. What is the type of `eval`? It's `Env -> Expr -> Value` where `Env = [(Id, Value)]`
2. We're working with `EVar x` so we also have `x` of type `Id`.
3. So at hand we have: an `Id`, an `Env` that is a `[(Id, Value)]`, and if we look at our helper functions we have a function `lookupId :: [(Id, Value)] -> Id -> Value`.
4. This looks like a good solution so let's start there and we see that `lookupId env x` works and does what it should do.

We can follow this for other base cases where it's sometimes even more straightforward.
For `eval env (EInt i)` we have `i` of type `Int`.
We also have `VInt` which takes an `Int` and produces a `Value`.
And indeed the implementation is `eval env (EInt i) = VInt i`.

### Contexts/Environments

`env` was very important to the previous example and these contexts/environments show up frequently in programming languages.
Whenever we are dealing with a variable, we probably want to do it using the environment.
We will see an example of this next when talking about `EApp`.

### Recursion

For cases with sub `Expr`essions: these will probably be recursive.
So anytime we see a case like `eval env (EApp e1 e2)` we're probably going to want to call `eval ___ e1` and `eval ___ e2`.
Keep these around as `eval` let's us produce `Value`s which is our eventual goal.

My first step for thinking about these "complex" expressions is to think of a specific example (doing the base cases first will help here), say `(\x -> x) 1` which we know should evaluate to `1`.
This case would look like
```
eval env (EApp (ELam "x" (EVar "x")) (EInt 1)) 
```
However, we are just using this to help us figure out how to write `eval env (EApp e1 e2)` so keep `e1 = (ELam "x" (EVar "x"))` and `e2 = (EInt 1)` in mind but don't focus too much on them.

At this point we have hopefully written the `eval` cases for `ELam` and `EInt`.
So we know that `eval env e1` in this case will return `VClos env "x" (EVar "x")`.
We also know that `eval env e2` will return `VInt 1`.

Now we see that we're dealing with a variable particularly the `"x"` argument of the closure.
A closure is a function values so it is waiting for an argument to substitute for `"x"`.
What does substitution mean though?
Here substitution means that, when we evaluate the body (`EVar x`), we want it to return `1`.
When does `eval ___ (EVar x)` return `1`?
Why when `___` has the binding `(x, VInt 1)`.

Now we have arrived at the partial implementation:
```
eval env (EApp e1 e2) = eval (x, v2):___ body
  where
    VClos closEnv x body = eval env e1
    v2 = eval env e2
```

Lastly we have to figure out what to put as the rest of the environment when evaluating `body`.
Here it is time to once again take stock of what types of things we have, in particular, what things of type `Env` we have (it's `env` and `closEnv`).
Now it pays to think about the case of `eval env (ELam x body) = VClos env x body`.
Remember that the `env` when evaluating `(ELam x body)` is the `closEnv` in the `EApp` case.
Here it pays to try to think about why we have `closEnv`.
It captures the environment when a function is defined so let's think about where that might be the case:
```
let x = 1 in
let f = \y -> x in
let x = 2 in
f 1
```
This should evaluate to `1`. Importantly for our implementation of `eval` for `EApp` it is the `closEnv` when defining the lambda that contains `(x, VInt 1)` whereas `env` at the application site contains `(x, VInt 2)`.
So we arrive at
```
eval env (EApp e1 e2) = eval (x, v2):closEnv body
  where
    VClos closEnv x body = eval env e1
    v2 = eval env e2
```

### Don't be more specific than you need to be

While we used `EInt 1` as the argument in our example we never had to look inside of it, all we had to do was call `eval env e2`.
You should try to do this whenever possible: if you don't need to know specific information about a subterm besides what it evaluates to (or whatever other thing you are computing) then just do the recursive call on it.

For non-recursive `ELet` we don't actually care what `e1` or `e2` in `let x = e1 in e2` are.
All we need to do is evaluate `e1` to `v1`, see that we have a variable, add `(x, v1)` to the environment, and then evaluate `e2` in the new environment.
In that description it never mattered what the actual expressions `e1` and `e2` are so we shouldn't pay attention to that.
Just treat them as abstract expressions.

This is in contrast to `e1` in the `EApp` case because there we need to know that the first thing evaluates to a function.
But notice that we only care that it *evaluates* to a function, so we only need to look at `e1` after evaluating it.
This is a good general rule for writing recursive functions for programming languages: poke the subterms with the function you're defining (here call `eval env e1`), *then* look at what you get back.

## WRITE AND RUN TESTS

When writing a case have a test in mind and run your code on it.
When we were writing the `eval` case for `EApp` above we had the test case `(\x -> x) 1` in mind.
After you write the code hop over to GHCI and run that test case and see if it does what you expect.
Don't wait until you've implemented every case and then test some large chunk of code because debugging that will be hard.

## Good luck.
