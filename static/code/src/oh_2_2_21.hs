

let EMPTY = \xs -> xs (\x y z -> FALSE) TRUE

                -- if xs == FALSE       we want to return TRUE
                -- if xs == (PAIR a b)  we want to return FALSE

let FALSE = (\x y -> y)

let PAIR x1 x2 = (\b -> b x1 x2)