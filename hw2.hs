import Data.Fixed (mod')
{- Derrick Pavia 
   CS431
   assignment 2 even Haskell problems -}

{- Derrick Problem 12 
   Write a Haskell function that produces the 
   list of the divisors of a given positive integer-}
divisors :: Int -> [Int] 
divisors n = [x| x <- [1..n], n `rem` x == 0]


