
module Oh_1_26 where

l0,l1,l2,l3 :: [Int]
l3 = 3:[]
l2 = 2:l3
l1 = 1:l2
l0 = 0:l1

bar :: [Int] -> Int
bar l = case l of
          [] -> -99
          h:_ -> h
-- >>> bar l0
-- 0
