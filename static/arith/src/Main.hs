import Language.Arith.Eval 
import System.Environment 

main :: IO ()
main = do 
  f:_ <- getArgs
  evalFile f
