inc :: Int -> Int 
inc x = x + 1



{- 
let FIX = ... 

let STEP = \rec -> \n -> ITE (ISZ n) ZERO (ADD n (rec (DEC n)))

let SUM  = FIX STEP 

eval ex_sum_two:

  SUM TWO 
  =*> STEP SUM TWO
  =*> ITE (ISZ TWO) ZERO (ADD TWO (SUM (DEC TWO))) 
  =*> ADD TWO (SUM (DEC TWO)) 
  =*> ADD TWO (SUM ONE) 
  =*> ADD TWO (ADD ONE (SUM ZERO))
  =*> ADD TWO (ADD ONE ZERO)





let SUM = \n -> ITE (ISZ n) ZERO (ADD n (SUM (DEC n)))


let SUM = \n -> FST (n 
                      (\p -> PAIR (ADD (FST p) (SND p)) (ADD (SND p) ONE))
                      (PAIR ZERO ZERO)))

def sum(n):

    (res,i) = (0, 0)

    repeat n times: 
    
    return res

-}
