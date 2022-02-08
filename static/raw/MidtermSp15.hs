-- | Midterm Sp15

module MidetermSp15 where

-- Problem 1
ans1a :: [Int]
ans1a =
    let x  = 0         in
    let a1 = let x = 1 in
             \y z -> [x, y, z]
    in
    let a2 = let x = 100 in
             a1 x
    in
    a2 x

data Tree a = Leaf
    | Node a (Tree a) (Tree a)

fold :: (a -> b -> b -> b) -> b -> Tree a -> b
fold f b Leaf         = b
fold f b (Node x l r) = f x (fold f b l) (fold f b r)

t0 :: Tree String
t0 = Node "cat" (Node "dog" Leaf Leaf) (Node "hippo" Leaf Leaf)

-- ans1b =
--     let f = (\_ vl vr -> 1 + vl + vr)
--     in fold f 0 t

ans1c :: String
ans1c =
    let f = (\x vl vr -> vl ++ x ++ vr)
    in fold f "" t0

data Option a = None | Some a

find :: (a -> Bool) -> [a] -> Option a
find f []      = None
find f (x:xs') = if f x then
                     Some x
                 else
                     find f xs'

xs0 :: [Int]
xs0 = [2, 4, 8, 16, 32]

ans1d :: Option Int
ans1d = let f x = x > 10 in find f xs0

ans1e :: Option Int
ans1e = let f x = (x `mod` 2) == 1 in
    find f xs0

-- Problem 2
data Set a = Set [a]

empty :: Show a => Set a
empty = Set []

member :: Eq a => a -> Set a -> Bool
member x (Set []) = False
member x (Set (x':xs)) = (x' == x) || member x (Set xs) -- TODO Do they know `||`?

add :: a -> Set a -> Set a
add x s = case s of
    Set xs -> Set (x:xs)

union :: Set a -> Set a -> Set a
union s1 (Set x2s) = foldl (\s x -> add x s) s1 x2s

del :: Eq a => a -> Set a -> Set a
del x (Set xs) = Set (filter (\y -> y /= x) xs)

s0 = empty
s1 = add 1 s0
s2 = add 2 s1
s3 = union s1 s2
s4 = del 1 s3

-- Problem 3
data Binop = Plus
    deriving Show

data Expr
    = Const Int
    | Var   String
    | Bin   Expr   Binop Expr
    | Let   String Expr Expr
    | App   Expr   Expr
    | Fun   String Expr
    deriving Show

e1 :: Expr
e1 = Bin (Const 1) Plus (Const 2)

e2 :: Expr
e2 = Let "x" (Const 1)
    (Let "y" (Const 2)
    (Bin (Var "x") Plus (Var "y")))

e3 :: Expr
e3 = Let "x" (Const 10)
    (App (Fun "y" (Bin (Var "x") Plus (Var "y")))  -- TODO There was a typo here in the exam I think?
         (Var "x"))

e1' :: Expr
e1' = Bin (Const 1) Plus (Var "x")

e2' :: Expr
e2' = Let "y" (Const 2)
     (Bin (Var "x") Plus (Var "y"))

e3' :: Expr
e3' = App (Let "z" (Const 10) (Fun "y" (Bin (Var "y") Plus (Var "z")))) (Var "z")

free :: Expr -> Set String
free (Var   x)        = Set ["x"]
free (Const n)        = Set []
free (Bin   e1 op e2) = union (free e1) (free e2)
free (App   e1 e2)    = union (free e1) (free e2)
free (Let   x  e1 e2) = union (free e1) (del x (free e2))
free (Fun   x  e1)    = del x (free e1)

isWellFormed :: Expr -> Bool
isWellFormed e = case free e of
    Set [] -> True
    _      -> False
