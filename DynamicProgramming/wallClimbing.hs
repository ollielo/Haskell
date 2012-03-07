import Data.Array
import Data.Tuple

-- Wall climbing example from Modification of CSC 364S Note, Univ of Toronoto, Fall 2003
climbing wall = (fst min_exit, (reverse . solution . snd) min_exit)
  where
    bnds  = bounds wall
    left  = snd $ fst bnds  -- left edge of the wall
    right = snd $ snd bnds  -- right edge of the wall
    top   = fst $ snd bnds  -- top edge of the wall
    -- table for minimum total cost at each brick
    table = array bnds [(ij, cost ij) | ij <- range bnds]
    -- the cost at the bottom of the wall
    cost (1, col)    = wall!(1, col)
    -- the minimum total cost at brick (i, j) is the minimum of the three lower
    -- bricks plus the cost of the current brick. 
    cost (row, col) 
      | col == left  = brick + min down lower_right
      | col == right = brick + min down lower_left
      | otherwise    = brick + minimum [lower_left, down, lower_right]
        where
          brick       = wall!(row, col)
          down        = table!(row-1, col)
          lower_left  = table!(row-1, col-1)
          lower_right = table!(row-1, col+1)
    -- the total cost of bricks at the top row
    top_row  = map (table!) (range ((top, left), (top, right)))
    -- find the brick with minimum total cost on the top row
    min_exit = minimum (zip top_row (range ((top, left), (top, right))))
    -- back tracing to find the path from top to bottom, it has to be reversed
    -- before printing.
    -- TODO: how can we use unfoldr/iterate/fix to do the recursion?
    solution (1, col) = [(1, col)]
    solution (row, col) = (row, col) : recur (row, col)
      where 
        recur (row, col)
          | diff == down       = solution (row-1, col)
          | diff == lower_left = solution (row-1, col-1)
          | otherwise          = solution (row-1, col+1)
            where
              diff        = (table!(row, col) - wall!(row, col))
              down        = table!(row-1, col)
              lower_left  = table!(row-1, col-1)
              lower_right = table!(row-1, col+1)

main :: IO ()
main = do
    let bricks = [3,2,5,4,8,5,7,5,6,1, 4,4,6,2,3,2,8,9,5,8]
        wall   = listArray ((1,1), (4,5)) bricks
    (putStrLn . show . climbing) wall
