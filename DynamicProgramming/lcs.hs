import Data.Array
import Debug.Trace

-- Navie solution to longest (maximal) common subsequence problem
lcs _ [] = (0, [])
lcs [] _ = (0, [])
lcs as@(x:xs) bs@(y:ys) 
  | x == y = (1 + fst match, x: snd match)
  | otherwise = max dropx dropy
  where
    match = lcs xs ys
    dropx = lcs xs bs
    dropy = lcs as ys

-- Dynamic programming solution to longest common subsequence problem
lcs' xs ys = (table!(m, n), reverse $ solution (m, n)) 
  where
    (m, n) = (length xs, length ys)
    bnds   = ((0, 0), (m, n))
    -- turn xs and ys into arrays for faster index access
    x      = listArray (1, m) xs
    y      = listArray (1, n) ys
    -- the table of maximal matches for the two strings
    table = array bnds [(ij, score ij) | ij <- range bnds]
    -- the score of any string against empty string is 0
    score (0, _) = 0
    score (_, 0) = 0
    -- get 1 score for match otherwise find out the best match by droping one
    -- character from either string.
    score (i, j)
      | x!i == y!j = 1 + table!(i-1, j-1)
      | otherwise = max (table!(i-1, j)) (table!(i, j-1))
    -- backtracing the maximum score table to find out the matching characters.
    solution (0, _) = []
    solution (_, 0) = []
    solution (i, j)
      | x!i == y!j  = x!i : solution (i-1, j-1)
      | table!(i-1, j) > table!(i, j-1) = solution (i-1, j)
      | otherwise = solution (i, j-1)

main :: IO ()
main = putStrLn $ show $ lcs' "appropriate meaning" "approximate matching"
