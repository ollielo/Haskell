import Data.Array

editDistance :: (Eq a) => [a] -> [a] -> Int
editDistance xs [] = length xs
editDistance [] ys = length ys
editDistance (x:xs) (y:ys) = minimum [a, b, c]
  where
    a = 1 + editDistance xs (y:ys)
    b = 1 + editDistance (x:xs) ys
    diff = if x == y then 0 else 1
    c = diff + editDistance xs ys

editDistance' xs ys = table ! (m, n)
  where
    (m, n) = (length xs, length ys)
    bnds   = ((0, 0), (m, n))
    --
    x      = listArray (1, m) xs
    y      = listArray (1, n) ys
    table  = array bnds [(ij, distance ij) | ij <- range bnds]
    -- the edit distance of a string against an empty string is the length of
    -- the string
    distance (i, 0) = i
    distance (0, j) = j
    -- the edit distance of current position is the minimum of either 
    -- 1. a match with penalty of 0
    -- 2. a mismatch with penalty of 1
    -- 3. inserting a gap in one of the strings with a penalty of 1 
    distance (i, j)
      | x!i == y!j = table ! (i-1,j-1)
      | otherwise = minimum [table ! (i-1, j) + 1,
                             table ! (i, j-1) + 1,
                             table ! (i-1, j-1) + 1]
main :: IO ()
main = putStrLn $ show $ editDistance' "appropriate meaning" "approximate matching"
