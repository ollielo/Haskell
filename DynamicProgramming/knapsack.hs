import Data.Array

-- solution to unbounded Knapsack problem, we can take any number of items as
-- long as the total weight does not exceed the maximum. 

-- items is a list of (value, weight) 
knapsack_ub :: (Ord a, Num a) => [(a, a)] -> a -> a
knapsack_ub items wmax = m wmax
  where
    m 0 = 0
    -- the 0 is needed because the list comprehension may result in empty
    -- list.
    m weight = maximum $ 0:[vi + m (weight - wi) | (vi, wi) <- items, 
                            wi <= weight]
    
knapsack_ub' items wmax = table!wmax
  where
    table    = array (0, wmax) [(weight, m weight) | weight <- [0..wmax]] 
    m 0      = 0
    m weight = maximum $ 0:[vi + table!(weight - wi) | (vi, wi) <- items, 
                            wi <= weight]

-- solution to bounded Knapsack problem, we can only pick one of each item from
-- the list.
knapsack items wmax = table!(nitems, wmax)
  where
    nitems  = length items
    values  = listArray (1, nitems) $ map fst items
    weights = listArray (1, nitems) $ map snd items  
    bnds    = ((0, 0), (nitems, wmax))
    table   = array bnds [(ij, profit ij) | ij <- range bnds]
    -- no profit when no item is picked
    profit (0, _) = 0
    -- no profit when weight limit is zero
    profit (_, 0) = 0
    profit (i, w)
    -- the weight of item i is larger than the current weight limit,
    -- we have no choice but to skip it
      | wi > w = table!(i-1, w)
    -- otherwise we can either pick it or not
      | otherwise = maximum [vi + table!(i-1, w - wi), table!(i-1, w)]
      where
        vi = values!i
        wi = weights!i

main :: IO ()
main = print $ knapsack [(30, 6), (14, 3), (16,4 ), (9, 2)] 10