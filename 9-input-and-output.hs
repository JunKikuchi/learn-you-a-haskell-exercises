{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}

import System.Environment
import System.Random
import Data.List
  
main = getArgs >>= putStr . concat . intersperse " " . echo
    where
        echo ("-n":xs) = xs
        echo xs        = xs ++ ["\n"]

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery = sort . take 6 . nub . randomRs (1, 49)
