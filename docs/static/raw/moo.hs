import Text.Printf (printf)
import System.Environment
import System.IO

main :: IO ()
main = do 
  cow <- readFile "cow.txt"
  msg <- getInput
  cowSays cow msg 

cowSays :: String -> String -> IO ()  
cowSays cow msg = do
  let out = blurb (msgLines lineSize msg) 
  putStrLn (out ++ cow)
  
getInput :: IO String
getInput = do
  args <- getArgs
  case args of 
    [] -> hGetContents stdin
    _  -> return (unwords args)

lineSize :: Int
lineSize = 40

blurb :: [String] -> String 
blurb lines = unlines  
  $  hdr 
  :  [ printf "< %s >" (pad l) | l <- lines ]
  ++ [ hdr ] 
  where 
    pad l = l ++ replicate (n - length l) ' '  
    n     = min lineSize (maximum (map length lines)) 
    hdr   = " " ++ replicate (n + 2) '-' ++ " "

msgLines :: Int -> String -> [String]
msgLines n str = go [] (words str)
  where 
    go acc []  = reverse acc  
    go acc ws  = let (l, rest) = splitAtSize n ws
                 in go (unwords l: acc) rest 

splitAtSize :: Int -> [String] -> ([String], [String])
splitAtSize n words  = go n [] words
  where 
    go k acc (w:ws) 
      | length w < k = go (k - length w - 1) (w:acc) ws
      | otherwise    = (reverse acc, w:ws)
    go k acc []      = (reverse acc, [])