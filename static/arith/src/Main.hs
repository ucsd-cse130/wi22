import Language.Arith.Eval 
import System.Environment 

main :: IO ()
main = do 
  putStrLn "Hello, world!"
  f:_ <- getArgs
  evalFile f
