import Cancellation
import Braids
import System.Environment

main :: IO ()
main = do
  input <- getArgs
  let (braid, mark) = parse input
  
  putStrLn "Here's kappa:"
  putStrLn . show . computeKappaNum $ braid
