import Cancellation
import Braids
import System.Environment

main :: IO ()
main = do
  input <- getArgs
  let (braid, mark) = parse input
  
  putStrLn "Here's kappa:"
  if mark == 0 then print (computeKappaNum braid)
    else if mark > 0 then print (computeReducedKappaNum braid mark)
        else then print (computeQuotientKappaNum braid (-mark))
