---
title: Logic Programming
headerImg: sea.jpg
---

## Logic Programming

**Imperative/functional programming**

- Program = Algorithm + Data Structures

- Execute

**Logic programming**

- Program = Facts + Rules

- Query

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Example

**Collection of Facts**

- Carnitas is Mexican 
- `isMexican(carnitas).`

**Collection of Rules**

- All Mexican food is delicious
- `isDelicious(X) :- isMexican(X).`

**Query**

- Hey Prolog! What is a delicious food ?
- `?- isDelicious(Y)`

**Answer**

- Carnitas!

<br>
<br>

You don't "run" a Prolog program, you *ask it questions*!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br> 

## Why Prolog?


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Prolog History

1970s: developed for *Artificial Intelligence*

  - original vision "expert systems"
  
Out came the whole field of **declarative programming**:  

  - Specify *what* you want

  - Not *how* to obtain result

  - Ideal for searching large space of results
  
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br> 


## Declarative Programming: Example

Flight search: Orbitz / Expedia / etc.

**Collection of Facts**

- Airports, Flights, Times, Durations, Costs

**Collection of Rules**

- *If* travel from `A` to `B` with price (`P1`) *AND* `B` to `C` with price (`P2`)
- *then* travel from `A` to `C` with price (`P1 + P2`)

**Queries**

- What is cheapest flight from SAN to JFK with duration < 6 Hrs ?


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br> 

## Declarative Programming: Applications

Used heavily in many domains (together with statistical methods)

- Scheduling
   
    - travel, sports, ...

- Rule-based Anomaly detection
    
    - Credit card fraud

- SQL (and similar DB Query Languages)

Many of these are inspired-by or are subsets of Prolog

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br> 

## Why Prolog?

<br>

Most importantly: 

yet another **new** way to think about programming!

<br>

(Remember the goal of this class: open your mind)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

Language

1. Terms
2. Facts
3. Queries
    - implementation: unification
4. Rules
5. Programs
    - implementation: backtracking search

Programming

- Numeric Computation
- Data Structures
- Puzzle Solving

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Language: Terms

Prolog Program

- Facts
- Rules
- Queries

... **about what** ? Terms!

<br>
<br>

Syntax of Prolog terms:

```haskell
t ::= n                -- number: 1, 92, 4.4
    | a                -- atom
    | X                -- variable
    | a (t1, t2, ..)   -- compound term
```

where 

* `a` is an identifier that starts with a lower-case letter
* `X` is an identifier that starts with an upper-case letter

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


### Prolog Terms: Atoms

**Atom:** any identifier starting with *lower*-case

**Examples:** `x`, `alice`, `taco`, `giraffe`, `appleSauce`

<br>
<br>

Prolog knows *NOTHING* about the atoms, except that:

  - Each atom is *equal to* itself
     - `alice = alice`
     - `taco = taco`

  - Each atem is *disequal to* every other atom
     - `alice = taco` *never* holds

<br>
<br>

You can think of atoms as constructors of a single mega *enum* type:

- `data Atom = x | alice | taco | giraffe | appleSauce | ...`
- except that in Haskell, constructors start with upper case :(

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Prolog Terms: Variables

**Variable:** any identifier starting with *upper*-case

- **Examples:** `X`, `Y`, `Z`, `Head`, `Tail`, `Taco`, `Burrito`, `Alice`, `Bob`

- `_` is the *wildcard* variable, similar to Haskell

<br>
<br>

A variable *stands for* (and can be *substituted with*) any term:

  - `alice = taco` *never* holds
  - but `X = taco` holds is we replace `[X = taco]`
  - what about `Alice = taco`?
  
(I) final

    `Alice = taco` holds with `[Alice = taco]`  

  
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>  


## Prolog Terms: Compound Terms

**Compound** terms are of the form 

`a (t1, t2, t3, ..)`

where

  - `a` is a lower-case string (a *function symbol*)
  - each `ti` is a term (an argument to the function symbol)


**Examples:**

```prolog
x(y, z)              % y, z       are atoms
parent(alice, bob)   % alice, bob are atoms
parent(alice, Child) % alice is an atom, Child is a variable
```


You can think of compound terms as trees:

<img src="../static/img/prolog_tree1.png" width=400 align="middle"/>

They are not function calls!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>  

## Prolog terms: Haskell representation

If we were *implementing* a Prolog interpreter in Haskell,
we would *represent* terms like so: 

```haskell
data Term
  = Num Int
  | Atom LId
  | Var UId
  | Compound LId [Term]
  
type LId = String -- lower-case
type UId = String -- upper-case
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
data Term
  = Num Int
  | Atom LId
  | Var UId
  | Compound LId [Term]
```  
  
What *Haskell* value of type `Term` represents *Prolog* term
`parent(alice, Bob)` ?

**A.** `parent ("alice", "Bob")`

**B.** `parent (Atom "alice") (Var "Bob")`

**C.** `[Atom "parent", Atom "alice", Var "Bob"]`

**D.** `Compound "parent" [Atom "alice", Var "Bob"]`

**E.** `Compound (Atom "parent") [Atom "alice", Var "Bob"]`

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

## QUIZ

```haskell
data Term
  = Num Int
  | Atom LId
  | Var UId
  | Compound LId [Term]
```  
  
What *Haskell* value of type `Term` represents *Prolog* term
`factorial(5)` ?

**A.** `factorial(5)`

**B.** `factorial(Atom 5)`

**C.** `120`

**D.** `Num 120`

**E.** `Compound "factorial" [Num 5]`

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


## Prolog Terms

Prolog term `factorial(5)` is simply the tree

<img src="../static/img/prolog_tree3.png" width=200 align="middle"/>

Prolog has **no idea** what `factorial` is (it's just a name)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br> 


## Plan

Language

1. ~~Terms~~
2. **Facts**
3. Queries
4. Rules
5. Programs

Programming

- Numeric Computation
- Data Structures
- Puzzle Solving

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Language: Facts

A **fact** is a "boolean" term (typically without variables) followed by `.`


**Examples:**

The following facts specify a list of *parent-child* relationships:

```prolog
parent(kim, holly).  
parent(margaret, kim).  
parent(herbert, margaret).
parent(john, kim).
parent(felix, john).  
parent(albert, felix).
parent(albert, dana).
parent(felix, maya).  
```

```
                  albert
                  |    |
herbert        felix    dana
    |         |    |
  margaret  john   maya
         |  |
         kim
          |    
        holly
```

Here: 

  * `kim`, `holly`, `margaret` etc. are all atoms
  * `parent` is a **predicate** (i.e. a boolean function symbol)
  * ... of **arity** 2 (i.e. takes two arguments)
  
<br>
<br>  
  
Predicates have *no meaning* to Prolog, but
the programmer *mentally notes* that:
    
  * `parent(kim, holly)`    **means** `kim` is a "parent-of" `holly`
  * `parent(margaret, kim)` **means** `margaret` is a "parent-of" `kim`
  * etc.
  

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>  

## Database of facts

Prolog maintains a database (collection) of facts

Suppose we have a collection of facts saved in [intro.pl](/static/raw/intro.pl)

You can **load** the facts into the Prolog interpreter

~~~~~{.prolog}
?- consult('intro.pl').
true.
~~~~~

... or you can **add** them one-at-a-time

~~~~~{.prolog}
?- assert(parent(margaret, kim)).
~~~~~

Once facts are loaded, you **query** Prolog

  * "Hey Prolog! Is this new fact **in** your Database ... or can it be **inferred** from your database?"
    
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>    

## Plan

Language

1. ~~Terms~~
2. ~~Facts~~
3. **Queries** (Implementation: Unification)
4. Rules
5. Programs (Implementation: Backtracking Search)

Programming

- Numeric Computation
- Data Structures
- Puzzle Solving

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Language: Query

A **query** is a boolean term (possibly with variables),
which we type into a prompt

<br>
<br>   

**Example 1:**

~~~~~{.prolog}
?- parent(margaret, kim).
~~~~~

Prolog replies:

~~~~~{.prolog}
true.
~~~~~

- As this was indeed one of the facts loaded in [intro](/static/raw/intro.pl)

<br>
<br>

**Example 2:**

~~~~~{.prolog}
?- parent(margaret, john).  
~~~~~

What does Prolog reply? Why?

<br>
<br>
<br>
<br>

The meaning of the query is "Is this fact **provable**"?

So the answer is: "No"

<br>
<br>


Pfft. Big deal? Is Prolog just a table lookup?!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Queries With Variables

What should Prolog reply to this query?

~~~~~{.prolog}
?- parent(margaret, X).
~~~~~

<br>
<br>
<br>
<br>
<br>
<br>

**Meaning:**

"Hey Prolog, for **which value(s)** of `X` is the fact **provable**"?

**Prolog replies:**

~~~~~{.prolog}
X = kim.
~~~~~

- Because it can *plug-in* `kim` for `X`,
- and then can infer `parent(margaret, kim)` from the facts

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## More examples

**Example 3:**

~~~~~{.prolog}
?- parent(X, kim).
~~~~~

(I) lecture
    
    Meaning: ???

(I) final

    Meaning: Who is the parent of `kim`?
    
<br>    
    
(I) lecture
    
    Prolog replies: ???
    
(I) final

    Prolog replies: `X = margaret; X = john.`  

<br>
<br>

**Example 4:**

~~~~~{.prolog}
?- parent(X, Y).
~~~~~

(I) lecture
    
    Meaning: ???

(I) final

    Meaning: What are all parent-child pairs in our database?

<br>    
    
(I) lecture
    
    Prolog replies: ???
    
(I) final

    Prolog replies: `X = holly, Y = kim; X = kim, Y = margaret; ...`  
    
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

How do we ask Prolog the following query:

> Does there exist **any** person who is their **own parent** ?

**A.** `parent(kim, kim)`

**B.** `parent(x, x)`

**C.** `parent(X, X)`

**D.** `parent(X, Y)`

**E.** `parent(Y, X)`

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
<br>

## More examples

**Example 4:**

How do we ask Prolog:

> Does there exist **any** person who has more than one child?

(I) final

    ```prolog
    parent(X,Y), parent(Y,Z), Y \= Z
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

## Queries work like magic

In Java/C#/Haskell you would need

- Some `parentOf` or `childOf` data structure / methods
    - to represent parent-child relationship

- Some looping or iteration
    - to search through all pairs

- Instead, Prolog uses *facts* and *queries*
    - to search *forwards* and *backwards*
    - to enumerate all results
    - in a single uniform *declarative* manner!
    
<br>

How?

<br>    

**Magic** = **Unification** + **Backtracking Search**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

Language

1. ~~Terms~~
2. ~~Facts~~
3. Queries
    - implementation: **unification**
4. Rules
5. Programs
    - implementation: backtracking search

Programming

- Numeric Computation
- Data Structures
- Puzzle Solving

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Unification

When does one term *match* (= *unify with*) another?

<br>
<br>
<br>
<br>
<br>

A term **unifies with** another term
when we can *substitute* values for their *variables* to make them *identical*

<br>
<br>

Unification is the computational heart of Prolog

<br>

When you write

~~~~~{.prolog}
?- term1 = term2.
~~~~~

you are asking whether `term1` *can be unified with* `term2`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Unification By Example

(I) lecture

    ```prolog
    ?- kim = kim.
    true.
    % atom is identical to itself!

    ?- kim = holly.
    false. 
    % different atoms are never identical

    ?- foo(kim) = foo(kim).
    true.
    % compound term: recursively unify subtrees

    ?- foo(kim) = foo(holly).
    ???

    ?- X = kim.
    X = kim.
    % X can be made identical to kim if we substitute [X = kim]

    ?- X = foo(kim).
    ???

    ?- foo(X) = foo(kim).
    ???

    ?- X = Y.
    ???
    ```
    
(I) final

    ```prolog
    ?- kim = kim.
    true.
    % atom is identical to itself!

    ?- kim = holly.
    false. 
    % different atoms are never identical

    ?- foo(kim) = foo(kim).
    true.
    % compound term: recursively unify subtrees

    ?- foo(kim) = foo(holly).
    false.

    ?- X = kim.
    X = kim.
    % X can be made identical to kim if we substitute [X = kim]

    ?- X = foo(kim).
    X = foo(kim).

    ?- foo(X) = foo(kim).
    X = kim.
    % compound term: recursively unify subtrees

    ?- X = Y.
    X = Y.
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

How does Prolog respond to the following query?

~~~~~{.prolog}
?- p(X, dog) = p(cat, Y).
~~~~~

**A.** `false`

**B.** `X = cat, Y = cat.`

**C.** `X = dog, Y = dog.`

**D.** `X = dog, Y = cat.`

**E.** `X = cat, Y = dog.`

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
<br>

## QUIZ

How does Prolog respond to the following query?

~~~~~{.prolog}
?- p(X, dog, X) = p(cat, Y, Y).
~~~~~

**A.** `false`

**B.** `X = cat, Y = cat.`

**C.** `X = dog, Y = dog.`

**D.** `X = dog, Y = cat.`

**E.** `X = cat, Y = dog.`

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


## Unification step-by-step

Let's see how Prolog reasons about this query:

~~~~~{.prolog}
?- p(X, dog, X) = p(cat, Y, Y).
~~~~~

Two compound terms: function symbols match, so recursively unify arguments...

<img src="../static/img/prolog_unify_1.png" width=500 align="middle"/>

<br>
<br>
<br>

To unify `X` and `cat`, extend the current *substitution* with `X = cat`...

<img src="../static/img/prolog_unify_2.png" width=500 align="middle"/>

<br>
<br>
<br>

Apply substitution `[X = cat]` to both terms. Move on to next leaf...

<img src="../static/img/prolog_unify_3.png" width=500 align="middle"/>

<br>
<br>
<br>

To unify `dog` and `Y`, extend the current *substitution* with `Y = dog`...

<img src="../static/img/prolog_unify_4.png" width=500 align="middle"/>

<br>
<br>
<br>

... and apply this substitution throughout *both* terms.

<img src="../static/img/prolog_unify_6.png" width=500 align="middle"/>

<br>
<br>
<br>

Uh oh! Now we have different atoms in the last leaf! **Unification fails.**

<img src="../static/img/prolog_unify_7.png" width=500 align="middle"/>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Unification in Prolog vs Nano?

It's the same thing, except!

| Unification in Nano          | Unification in Prolog |
| ---------------------------- | --------------------- |
| on two *types*               | on two *terms*        |
| is a pain to implement       | is built-in           |

<br>
<br>

So, if we implemented Nano type inference in Prolog instead of Haskell,
we wouldn't have to implement `unify`, we could just use `=`!

```prolog
?- int = int.
true.

?- int = bool.
false. 

?- funT(int,A) = funT(B,bool).
A = bool, B = int.

?- A = funT(A,A).
???
```

(I) final

    By default, Prolog doesn't perform the occurs check for efficiency reason, so it answers `A = funT(A,A)`.
    You can enable it with `set_prolog_flag(occurs_check, true).`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Unification for answering queries

What happens when we ask

~~~~~{.prolog}
?- parent(margaret, X).  
~~~~~

- Prolog goes through the list of *facts* in its database

- *For each fact*, checking if it can be *unified* with the query

  - If unification *fails* it moves on to the next fact

  - If unification *succeeds* it returns *the unifier* (the unifying substitution)

    - if the unifier is empty, it just replies `true`
    
- If unification fails for each fact, it relies `false`
  
<br>
<br>
<br>  
  
**More examples**

~~~~~{.prolog}
?- parent(X, kim).  
~~~~~

This query has multiple solutions!

  - Once the first solution is found, we can ask Prolog to keep going through the facts
  
  
<br>
<br>  

~~~~~{.prolog}
?- parent(X, Y).  
~~~~~

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

Language

1. ~~Terms~~
2. ~~Facts~~
3. ~~Queries~~
    - implementation: unification
4. **Rules**
5. Programs
    - implementation: backtracking search

Programming

- Numeric Computation
- Data Structures
- Puzzle Solving

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Complex queries

How do we determine if `margaret` is the *grandparent* of `holly`?

<br>
<br>
<br>
<br>
<br>

We use a **conjunction** (comma-separated sequence of terms):


~~~~~{.prolog}
?- parent(margaret, X), parent(X, holly).
~~~~~

Is there a person `X` who is a child of `margaret` **AND** a parent of `holly` ?

**Note:** how we link the terms with the same variable to capture relationships.

<br>
<br>

How do we determine who is the *great-grandparent* of `holly`?

<br>
<br>
<br>
<br>
<br>

~~~~~{.prolog}
?- parent(GGP, GP), parent(GP,P), parent(P, holly).
~~~~~

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

Which of these queries returns all pairs `X`, `Y` of (half-) **siblings**?

**A.** `parent(X, Y), X \= Y.`

**B.** `parent(X, Z), parent(Z, Y), X \= Y.`

**C.** `parent(X, Z), parent(Y, Z), X \= Y.`

**D.** `parent(Z, X), parent(Z, Y), X \= Y.`

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


## Complex queries

~~~~~{.prolog}
?- parent(GGP, GP), parent(GP,P), parent(P, holly).
~~~~~

This is getting a tad repetitive!

What do we do in other languages to avoid repeating ourselves?

<br>
<br>
<br>
<br>

Abstract things into functions!

In Prolog, they are called **rules**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Rules

Rules let us build *complex predicates* from *queries*

<br>

**Syntax:**

~~~~~{.prolog}
t :- t1, t1, t2,...
~~~~~

- `t` is called the **head** of the rule
- `t1, t2, t3,...` is called the **body** of the rule
- all `t, t1, t2, t3,...` are predicate applications

<br>

**Intuition:**

To prove the head `t`, you must prove all sub-goals `t1` **AND** `t2` **AND** `t3` ...

<br>

**Example:**

We can build a new (complex) predicate `grandparent` from `parent` predicates:

~~~~~{.prolog}
grandparent(GP, GC) :- parent(GP, P), parent(P, GC).
~~~~~

`GP` is a *grandparent* of `GC` if `GP` is a *parent* of `P` **and** `P` is a *parent* of `GC`

<br>
<br>
<br>
<br>

Now we can use the new predicate in a query:

~~~~~{.prolog}
?- grandparent(X, kim). % who are the grandparents of kim
X = herbert ;           % hit ; to see next
X = felix   ;           % hit ; to see next
false.                  % thats it!
~~~~~

<br>
<br>
<br>
<br>

Why? Because Prolog can prove the sub-goals:

~~~~~{.prolog}
?- parent(herbert, P), parent(P, kim).  % Solution 1. X = herbert
P = margaret.

?- parent(felix, P), parent(P, kim).    % Solution 2. X = felix
P = john .
~~~~~

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

Which of the following is a valid `greatgrandparent` predicate?

**A.** `greatgrandparent(X, Y) :- parent(X, Y), grandparent(X, Y).`

**B.** `greatgrandparent(X, Y) :- parent(X, Z), grandparent(Z, Y).`

**C.** `greatgrandparent(X, Y) :- grandparent(X, Z), parent(Z, Y).`

**D.** `greatgrandparent(X, Y) :- parent(X, Z), parent(Z, Y).`

**E.** `greatgrandparent(X, Y) :- parent(X, Z1), parent(Z1, Z2), parent(Z2, Y).`

<br>

(I) final

    *Answer:* B, C, or D

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Rules: complex predicates from queries

~~~~~{.prolog}
greatgrandparent(GGP, GC) :- parent(GGP, GP), grandparent(GP, GC).
~~~~~

That was your first Prolog program!

Let's take it out for a spin!

~~~~~{.prolog}
?- greatgrandparent(X, holly).
X = herbert ;
X = felix ;
false.
~~~~~

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

Language

1. ~~Terms~~
2. ~~Facts~~
3. ~~Queries~~
    - ~~implementation: unification~~
4. ~~Rules~~
5. **Programs**
    - implementation: backtracking search

Programming

- Numeric Computation
- Data Structures
- Puzzle Solving

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Prolog Programs

A **program** is a collection of *facts* and *rules*

Facts and Rules are two kinds of **clauses**:

- **Fact:** clause **without** any conditions

- **Rule:** clause **with** conditions

    - used to derive new facts

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Program structure

How do combine multiple rules into a program?

1. Scope

2. Disjunction

3. Recursion

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Scope

In the `grandparent` rule, the variable `GP` appears *twice*:

~~~~~{.prolog}
greatgrandparent(GGP, GC) :- parent(GGP, GP), grandparent(GP, GC).
~~~~~

Is it the same `GP`?

(I) final

    *Yes*


<br>
<br>
<br>
<br>

Below we use `X` in *both* rules:

~~~~~{.prolog}
isDelicious(X)  :- isMexican(X).
isVegetarian(X) :- isVegan(X).
~~~~~

Is is the same `X`?

(I) final

    *No*


<br>
<br>
<br>
<br>

In Prolog, the **scope** of a variable is the *single* rule where it appears.

There is *no connection* between variables *across*  rules.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Program structure

How do combine multiple rules into a program?

1. Scope

2. **Disjunction**

3. Recursion

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Complex Predicates: Disjunction

Lets write a predicate `has_family` which is true for persons who

- **either** have a parent

- **or** have a child

<br>
<br>
<br>
<br>
<br>

~~~~~{.prolog}
has_family(X) :- parent(X, _). % if X is the parent of some _
has_family(X) :- parent(_, X). % if X is the child of some _
~~~~~

`_` is a *wildcard* or *dont-care* variable (as in Haskell)

<br>

**Intuition:**

- *If* we can prove `parent(X, _)` *then* we can prove `has_family(X)`

- *And also*: *if* we can prove `parent(_, X)` *then* we can prove `has_family(X)`

<br>

**Running the program:**

~~~~~{.prolog}
?- has_family(holly).
true.  % Second rule fires for holly

?- has_family(sansa).
false. % Neither rule fires for sansa
~~~~~


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Complex Predicates: Disjunction


~~~~~{.prolog}
has_family(X) :- parent(X, _). % if X is the parent of some _
has_family(X) :- parent(_, X). % if X is the child of some _
~~~~~

Can be abbreviated to

~~~~~{.prolog}
has_family(X) :- parent(X, _) ; parent(_, X).
~~~~~

Semicolon `;` indicates disjunction.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Program structure

How do combine multiple rules into a program?

1. Scope

2. Disjunction

3. **Recursion**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recursion

Lets write a predicate `ancestor(A, X)` which is true if

- `parent(A, X)` ... **or**

- `parent(A, P)` and `parent(P, X)` ... **or**

- `parent(A, GP)` and `parent(GP, P)` and `parent(P, X)` ... **or**

- ... if **some** chain of parent-links holds between `A` and `X`.

<br>
<br>
<br>

`A` is an ancestor of `X` if:

  - *Base case:* `A` is a parent of `X`
  
    `ancestor(A, X) :- parent(A, X).`
  
  - *Inductive case:* `A` is an ancestor of `X`'s parent
  
    `ancestor(A, X) :- parent(P, X), ancestor(A, P).`
  
<br>
<br>
<br>
<br>
  
To sum up:

```prolog
ancestor(A, X) :- parent(A, X).
ancestor(A, X) :- parent(P, X), ancestor(A, P).
```

What can we do with this predicate?
  
<br>
<br>
<br>
<br>

We can find *descendants* (forwards)

~~~~~{.prolog}
?- ancestor(kim, X).
X = holly.
~~~~~

<br>
<br>
<br>
<br>

We can find *ancestors* (backwards)

~~~~~{.prolog}
?- ancestor(X,kim).
X = margaret  ;
X = john      ;
X = herbert   ;
X = felix     ;
X = albert    .
~~~~~

<br>
<br>
<br>
<br>
<br>


Pretty neat: search forward or back, in just two lines!

  - Try doing that in any other language!
  
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

Language

1. ~~Terms~~
2. ~~Facts~~
3. ~~Queries~~
    - ~~implementation: unification~~
4. ~~Rules~~
5. Programs
    - implementation: **backtracking search**

Programming

- Numeric Computation
- Data Structures
- Puzzle Solving  

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
  
## Backtracking Search

How does Prolog answer recursive queries?

- Brute force *backtracking search*

View each clause as a **proof rule**:

```prolog
goal :- subgoal_1, subgoal_2, ...
goal :- subgoal_3, subgoal_4, ...
...
```

To prove `goal`, try to:

1. Prove `subgoal_1` **and then** prove `subgoal_2` etc, or, **failing that**,
2. Prove `subgoal_3` **and then** prove `subgoal_4`, etc, or, **failing that**...

<br>
<br>

**Example:**

```prolog
ancestor(A,X) :- parent(A,X).
ancestor(A,X) :- parent(P,X), ancestor(A,P).
```

To prove `ancestor(A,X)`, try to:

1. Prove `parent(A,X)`, or, **failing that**,
2. Prove `parent(P,X)`, **and then** prove `ancestor(A,P)`.

<br>
<br>

Suppose we ask it the query:

```prolog
?- ancestor(felix,holly).
```

To prove this query, it undertakes the following backtracking search:

```prolog
    ancestor(felix,holly)?
      /                    \
parent(felix,holly)    parent(P0,holly)
  NO                   ancestor(felix,P0)
                       |
      ------------------
      |
      | P0=kim  (by fact)
      |
      ancestor(felix,kim)
      /                \
  parent(felix,kim)     parent(P1,kim)
      NO                ancestor(felix,P1)
                         |              |
           P1 = margaret |              | P1 = john
                         |              |                         
        ancestor(felix,margaret)        ancestor(felix,john)
                /        \                        |
parent(felix,margaret)   parent(P2,margaret)   parent(felix,john)
            NO           ancestor(felix,P2)         YES
                                    |
                       P2 = herbert |
                                    |
                      ancestor(felix, herbert)
                     /              |
   parent(felix,herbert)   parent(P3,herbert)
              NO                   NO
```

<br>
<br>

**NOTE:** Prolog introduces a fresh `P` variable (`P1, P2, P3...`)
every time the second rule for `ancestor` is triggered 

<br>
<br>

**HINT:** Trace mode in Prolog shows the search tree!

```prolog
?- trace.
?- ancestor(felix,holly).
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

## Queries with Variables

*Backtracking Search* is done for **every** query.

```prolog
  ?- ancestor(X,kim).
```

- Prolog does the proof search
- Returns **all** unifiers for `X` for which the proof succeeds with `YES`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Order matters

Order of clauses & terms influences unification & backtracking.

For each

- **goal** different clauses are selected in order
- **clause** subgoals are solved (and unified!) *from left-to-right*

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

Which alternative definitions of `ancestor` are correct?

```prolog
% A
ancestor(A,X) :- parent(P,X), ancestor(A,P).
ancestor(A,X) :- parent(A,X).

% B
ancestor(A,X) :- parent(A,X).
ancestor(A,X) :- ancestor(A,P), parent(P,X).

% C
ancestor(A,X) :- ancestor(A,P), parent(P,X).
ancestor(A,X) :- parent(A,X).

% D: All of the above

% E: None of the above
```

<br>

(I) final

    *Answer:* A is correct, but less efficient. B and C loop forever


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Bad orders

A bad order of sub-goals of in a recursive rule can cause **non-termination**

If we define:

```prolog
ancestor(A,X) :- ancestor(A,P), parent(P,X).
ancestor(A,X) :- parent(A,X).
```

The following query doesn't terminate:

```prolog
?- ancestor(felix,holly).
```

<br>
<br>

Why? The search tree looks like this now!

```prolog
  ancestor(felix,holly)?
    |
    |
    |
  ancestor(felix,P0)  %prove first subgoal,
    |                 %then parent(P,holly)
    |
    |
  ancestor(felix,P1)  %prove first subgoal,
    |	                %then parent(P1,P0)
    |
    |
  ancestor(felix,P2)
    .
    .
    .
```

**To avoid infinite recursion:**

- Always place the base-case subgoal *first* in a recursive rule

- Then unification with the base facts *fixes* the variable `P`

- Thereby guaranteeing termination


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

What is the best definition of `sibling`?

- where `sibling(X, Y)` if `X` and `Y` share a parent.

```prolog
% A
sibling(X, Y) :- parent(P, X), parent(P, Y).

% B
sibling(X, Y) :- parent(P, X), parent(P, Y), not(X = Y).

% C
sibling(X, Y) :- not(X = Y), parent(P, X), parent(P, Y).
```

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
<br>


## Ordering and Unification

Remember: `=` means *unification*!

So, `X = Y` *always holds*:

~~~~~{.prolog}
?- X = Y.
X = Y
~~~~~

Hence, `not(X=Y)` (which is the same as `X \= Y`) *always fails*!

~~~~~{.prolog}
?- not(X = Y).
false.
~~~~~

So, if we define:

~~~~~{.prolog}
sibling(X, Y) :- not(X = Y), parent(P, X), parent(P, Y).
~~~~~

Then `sibling(X,Y)` (with variables for inputs) always fails!

<br>
<br>

**Solution**

Always ensure the sub-goal `not(X=Y)` fires *after* `X` and `Y` have been fully instantiated:

<br>
<br>

~~~~~{.prolog}
sibling(X, Y) :- parent(P, X), parent(P, Y), not(X = Y).
~~~~~

and now we get:

~~~~~{.prolog}
    ?- sibling(X,Y).
    X = john
    Y = maya ;

    X = felix
    Y = dana ;

    X = dana
    Y = felix ;

    X = maya
    Y = john ;
    No
~~~~~

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

~~Language~~

**Programming**

- **Numeric Computation**
- Data Structures
- Puzzle Solving

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>  

## Numeric Computation

Two big problems:

1. How do we even *evaluate*? e.g. `2 + 3` ?

2. How do we write *functions*? e.g. `add x y = x + y`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br> 

### Problem 1: How to Evaluate?

Everything is a *term* and `=` is a *unification* operator:

~~~~~{.prolog}
?- X = 2 + 3.
X = 2 + 3
~~~~~

But to "compute" we need to be able to *evaluate* `2 + 3` to `5`!

<br>
<br>

Prolog has a magic operator called `is` that *evaluates* its second argument:

~~~~~{.prolog}
?- X is 2 + 3.
X = 5.
~~~~~

<br>
<br>

To solve the goal `t1 is t2`, Prolog:

1. **evaluates** `t2` and _then_
2. **unifies** result with `t1`

<br>
<br>

All variables in `t2` must be instantiated *before* evaluation!

~~~~~{.prolog}
% Not good:
?- Y is X+2, X=1.
ERROR: Args are not sufficiently instantiated

% This is okay:
?- X=1, Y is X+2.
X=1
Y=3
~~~~~

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Problem 2: How to Write Functions?

In Prolog we can define *predicates* using *clauses* (facts and rules)

Can we represent a *function definition* like

```haskell
add x y = x + y
```

using predicates and clauses?

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

Which of the following represents `add x y = x + y`?

~~~~~{.prolog}
% A
addP(X, Y) :- Z is X + Y.

% B
addP(X, Y, Z) :- Z is X + Y.

% C
addP(X, Y, X + Y).

% D
addP(X, Y) :- X + Y.

% E
addP(X, Y, Z) :- X + Y is Z.
~~~~~

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
<br>

## Functions as Predicates

Every **function** of the form:

```haskell
foo x y = Res
```

corresponds to a **predicate** of the form:

```prolog
fooP(X, Y, Res).
```

i.e. a predicate that is `True` for those triples `(X, Y, Res)` s.t.
the function `foo X Y` evaluated to `Res`

The predicate captures the **input-output relation** of the function.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


### Example: Fibonacci

Recall the definition of Fibonacci numbers in Haskell:

```haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n -2)
```

<br>

Lets write a Prolog predicate capturing the IO relationship of `fib`:

~~~~~{.prolog}
fib(N, Res)
~~~~~

holds only when `Res` is the `N-th` Fibonacci number.

When we are done, we can **call** the function with a query:

~~~~~{.prolog}
?- fib(0, Res).
Res = 1

?- fib(5, Res).
Res = 8
~~~~~  

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

What is the correct Prolog implementation of factorial?

~~~~~{.prolog}
% A
fact(1,1). 
fact(N,R) :- N1 is N-1, fact(N1,R1), R is N*R1.

% B
fact(N,1) :- N < 2.
fact(N,R) :- N1 is N-1, fact(N1,R1), R is N*R1.

% C
fact(N,1) :- N < 2.
fact(N,R) :- 1 < N, N1 is N-1, fact(N1,R1), R is N*R1.

% D
fact(N,R) :- 1 < N, N1 is N-1, fact(N1,R1), R is N*R1.
fact(N,1) :- N < 2.
~~~~~

<br>

(I) final

    *Answer:* C and D both fine (order of clauses doesn't matter with mutually exclusive guards!)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

~~Language~~

**Programming**

- ~~Numeric Computation~~
- **Data Structures**
- Puzzle Solving


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Lists

Prolog has built-in syntax for lists

```prolog
?- [H|T] = [1,2,3].
H = 1,
T = [2,3].
```

<br>

Feels a lot like Haskell pattern matching

  * no wonder: pattern matching **is** unification!
  
<br>  
  
Unlike Haskell, can mix `,` and `|`:

```prolog
?- [X,Y|T] = [1,2,3].
X = 1,
Y = 2,
T = [3].
```  
  
<br>
<br>
<br>
<br>

### Head and tail

We have to write them as predicates:

```prolog
headOf([H|_],H).

tailOf([_|T],T).
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

How do we write a predicate `hasThreeOrMore` that satisfies the following test cases:

```prolog
?-hasThreeOrMore([]).
false.
?-hasThreeOrMore([1,two]).
false.
?-hasThreeOrMore([1,two,dog]).
true.
?-hasThreeOrMore([1,two,dog,taco]).
true.
```

**A.** `hasThreeOrMore([_|_]).`

**B.** `hasThreeOrMore([_,_|_]).`

**C.** `hasThreeOrMore([_,_,_|_]).`

**D.** `hasThreeOrMore([_,_,_,_|_]).`

**E.** `hasThreeOrMore([_,_,_]).`

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
<br>

## Recursive functions on lists

**Example 1:** `isIn`

Let's write the Prolog equivalent of the following Haskell function:

```haskell
isIn :: a -> [a] -> Bool
isIn _ []     = False
isIn x (y:ys) = x == y || isIn x ys
```

<br>
<br>
<br>
<br>
<br>

```prolog
isIn(X,[X|_]).
isIn(X,[_|T]) :- isIn(X,T).
```

<br>
<br>
<br>
<br>
<br>

**Example 2:** `sum`

Let's write the Prolog equivalent of the following Haskell function:

```haskell
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs
```

<br>
<br>
<br>
<br>
<br>

```prolog
sum([],0).
sum([H|T],R) :- sum(T,R1), R is R1 + H.
```

<br>
<br>
<br>
<br>
<br>

**Example 3:** `append`

Let's write the Prolog equivalent of the following Haskell function:

```haskell
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x:(append xs ys)
```

<br>
<br>
<br>
<br>
<br>

```prolog
append([],Ys,Ys).
append([H|T],Ys,[H|R]) :- append(T,Ys,R).
```

<br>
<br>

Unlike in Haskell, Prolog's append is **magical!**

```prolog
?- append(Xs,Ys,[1,2,3,4]).
Xs = [], Ys = [1,2,3,4] ;
Xs = [1], Ys = [2,3,4]  ;
Xs = [1,2], Ys = [3,4]  ;
Xs = [1,2,3], Ys = [4]  ;
Xs = [1,2,3,4], Ys = [] .
```

<br>
<br>

Prolog has a built-in library of list functions, including `append`, `reverse`, etc

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

~~Language~~

**Programming**

- ~~Numeric Computation~~
- ~~Data Structures~~
- Puzzle Solving


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Farmer, Wolf, Goat, Cabbage

![The real puzzle](/static/img/logic_boat.png){#fig:puzzle .align-center}

Farmer needs to end up on the east bank of the river.

He has a little boat that can only carry him and one of the goods.

Goat can't be left alone with the cabbage, wolf can't be left alone with the goat.

Let's ask Prolog to solve it for us!

<br>
<br>

We will encode the state of the game as a list with four positions:

```prolog
[P_Farmer,P_Wolf,P_Goat,P_Cab]
```

Each position can take on one of the two atoms: `east` or `west`.

<br>
<br>

First, we define four legal moves that have the farmer at at most one good in the boat:

```prolog
move([X,X,P_Goat,P_Cab],     move_wolf,   [Y,Y,P_Goat,P_Cab])      
  :- change(X,Y).
move([X,P_Wolf,X,P_Cab],     move_goat,   [Y,P_Wolf,Y,P_Cab])      
  :- change(X,Y).
move([X,P_Wolf,P_Goat,X],    move_cabbage,[Y,P_Wolf,P_Goat,Y])     
  :- change(X,Y).
move([X,P_Wolf,P_Goat,P_Cab],move_nothing,[Y,P_Wolf,P_Goat,P_Cab]) 
  :- change(X,Y).

change(east,west).
change(west,east).
```

<br>
<br>

Next, we define the safety condition:

```prolog
safe([P_Farmer,P_Wolf,P_Goat,P_Cab]) :-
  one_equal(P_Farmer,P_Wolf,P_Goat),
  one_equal(P_Farmer,P_Goat,P_Cab).
  
one_equal(X,X,_).
one_equal(X,_,X).
```

<br>
<br>

Finally, we define a solution as a sequence of legal moves that ends in the final state:

```prolog
solution([east,east,east,east],[]).
solution(State,[FirstMove|RemainingMoves]) :-
  move(State,FirstMove,NextState),
  safe(NextState),
  solution(NextState,RemainingMoves).
```

<br>
<br>

Now we can ask Prolog to solve the puzzle like so:

```prolog
?- length(Moves,7), solution([west,west,west,west],Moves).
```

<br>
<br>

Why do we need `length(Moves,7)`?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Backtracking control

Sometimes we want more fine-grained control over the backtracking

<br>

For example:

```prolog
?- length(Moves,7), solution([west,west,west,west],Moves).
```

gives us a gazillion solutions, most that all look the same (-ish?) 

<br>
<br>
<br>

### Stop backtracking

The **cut** operator `!` is a goal that

  - always succeeds
  - cannot be back-tracked out of
  
<br>
<br>
  
We can use cut to tell Prolog: 
once you found a solution to the puzzle, look no further!

```prolog
solution([east,east,east,east],[]).
solution(State,[FirstMove|RemainingMoves]) :-
  move(State,FirstMove,NextState),
  safe(NextState),
  solution(NextState,RemainingMoves), !.
```  

<br>
<br>
<br>
<br>

### Collect all solutions

Sometimes you want to **localize** the backtracking
and collect all solutions in a list.

```prolog
?- isIn(X,[2,1,3,2]).
X = 2.
X = 1.
X = 3.
X = 2.
```

<br>
<br>

Use `bagof` to get the list of all solutions:

```prolog
?- bagof(X, isIn(X,[2,1,3,2]), Sols).
Sols = [2,1,3,2].
```

<br>
<br>

Use `setof` to get the list of all *unique* solutions:

```prolog
?- setof(X, isIn(X,[2,1,3,2]), Sols).
Sols = [1,2,3].
```

<br>
<br>

How do we find all *unique* solutions to the farmer puzzle?

(I) final

    `?- setof(_, (length(Moves,7), solution([west,west,west,west],Moves)), _).`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

That's all folks!
