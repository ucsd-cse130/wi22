
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

lastElem :: [a] -> a
lastElem [x]   = x
lastElem (_:t) = lastElem t
lastElem _     = error "oops-lastElem-called-with-empty" 


{- 
 x = (1, "two", 3.3)
 y = (12, 19)

 x[0]
 x[1]
 x[2]
 x[i]
 y[0]

   "10" + 2


    x.foo(args)

    x.__add__(y)

[2004]
    map - reduce 

[1,2,3,4] ==> [10,20,30,40]

times10 []    = []
times10 (h:t) = h*10 : times10 t

[1,2,3,4] ==> ["1","2","3","4"]

toStrings []    = []
toStrings (h:t) = show h : toStrings t

map op []    = []
map op (h:t) = op h : map op t 

times10   = map (\n -> n*10)
toStrings = map (\n -> show n)


tail-rec


int sumArray(Collection<Int> a) {
  int tot = 0;
  for (int i = 0; i++; i < 1000) {
    tot += a[i]
  }
  return tot;
}

-}