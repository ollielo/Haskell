import Debug.Trace
import Data.Array

-- longest increasing subsequence
lis xs = table
  where
    n = length xs
    bnds = (1, n)
    x = listArray bnds xs
    table = array bnds [(i, score i) | i <- range bnds]
    --
    score i = sc smallers
      where
        smallers = [j | j <- [1..i-1], x!j < x!i]
        sc [] = 1
        sc js = 1 + (maximum $ map (table!) js)

main :: IO()
main = putStrLn (show $ lis [5, 2, 8, 6, 3, 6, 9, 7])