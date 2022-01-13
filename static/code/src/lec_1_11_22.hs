inc :: Int -> Int 
inc x = x + 1



{- 

let SUM = \n -> ITE (ISZ n) ZERO (ADD n (SUM (DEC n)))


let SUM = \n -> FST (n 
                      (\p -> PAIR (ADD (FST p) (SND p)) (ADD (SND p) ONE))
                      (PAIR ZERO ZERO)))

def sum(n):

    (res,i) = (0, 0)

    repeat n times: 
    
    return res

-}
