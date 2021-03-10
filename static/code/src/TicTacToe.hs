module TicTacToe where

import qualified Data.Map as M
import Text.Read (readMaybe)

data Player = PO | PX 
            deriving (Eq, Ord, Show)

data Pos = Zero | One | Two
           deriving (Eq, Ord, Show, Read)

type Move = (Pos, Pos)

type Board = M.Map Move Player

data State = State { sBoard :: Board, sPlayer :: Player }
             deriving (Eq, Ord, Show)

emptyBoard :: Board
emptyBoard = mempty

isValid :: Board -> Move -> Bool
isValid b m = M.notMember m b

updateBoard :: Board -> Player -> Move -> Board 
updateBoard b p m = M.insert m p b

isWinner :: Board -> Maybe Player
isWinner b = firstMaybe (isWin b <$> wins)

isWin :: Board -> Win -> Maybe Player
isWin b [pos1, pos2, pos3] = do
    p1 <- M.lookup pos1 b 
    p2 <- M.lookup pos2 b 
    p3 <- M.lookup pos3 b 
    if (p1 == p2 && p2 == p3) 
        then Just p1 
        else Nothing


firstMaybe :: [Maybe a] -> Maybe a
firstMaybe (Nothing : xs) = firstMaybe xs
firstMaybe (Just x: _) = Just x 
firstMaybe [] = Nothing


-- >>> wins
-- [[(Zero,Zero),(Zero,One),(Zero,Two)],[(One,Zero),(One,One),(One,Two)],[(Two,Zero),(Two,One),(Two,Two)],[(Zero,Zero),(One,Zero),(Two,Zero)],[(Zero,One),(One,One),(Two,One)],[(Zero,Two),(One,Two),(Two,Two)],[(Zero,Zero),(One,One),(Two,Two)],[(Zero,Two),(One,One),(Two,Zero)]]

type Win = [(Pos, Pos)]

wins :: [Win]
wins = rows ++ cols ++ [ldiag] ++ [rdiag]
  where 
      rows  = [ [ (r, c) | c <- alls ] | r <- alls ]
      cols  = [ [ (r, c) | r <- alls ] | c <- alls ]
      ldiag = [ (i, i) | i <- alls ]
      rdiag = [ (Zero, Two), (One, One), (Two, Zero)]

alls :: [Pos]
alls  = [Zero, One, Two]

showBoard :: Board -> String
showBoard b = unlines [showRow b r | r <- alls]

showRow b r = concat [showPos b r c | c <- alls]

showPos b r c = case M.lookup (r, c) b of
                    Just PX -> "X"
                    Just PO -> "O"
                    Nothing -> "#"


isOver :: Board -> Bool
isOver b = M.size b == 9

getMove :: Player -> IO Move
getMove p = do 
    putStrLn ("Your move " ++ show p)
    putStrLn "(row, col)?"
    l <- getLine 
    case readMaybe l of 
        Just m -> return m
        _ -> getMove p

exit :: Maybe Player -> IO ()
exit Nothing = putStrLn "Boo! Draw"
exit (Just p)= putStrLn ("Winner! " ++ show p)

other :: Player -> Player
other PX = PO
other PO = PX

loop :: Board -> Player -> IO ()
loop board player 
  | isOver board             = exit Nothing
  | Just p <- isWinner board = exit (Just p)
  | otherwise = do 
      putStrLn (showBoard board)
      move   <- getMove player
      if not (isValid board move) 
          then loop board player
          else loop (updateBoard board player move) (other player)

-- [
--     main :: S -> T 
--     ...
--         foo 
--         bar 
--         baz
--     foo :: S1
--     bar :: S2
--     baz :: S3
-- ] 



{- 

copyKeys mSrc mDst = mDst + (mSrc - mDst)

-}


main :: IO ()
main = loop emptyBoard PX







