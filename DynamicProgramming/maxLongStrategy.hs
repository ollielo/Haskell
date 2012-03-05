import Data.Array

-- Exercise 6.7 in Algorithm Design
-- Finding the maximum profit with a long (buy then sell) trade for a given 
-- sequence of prices.

maxLongStrategy ps = table
  where
    -- bounds for tables
    n      = length ps
    bnds   = (0, n-1)
    -- turn ps into Array for faster indexing
    prices = listArray bnds ps
    -- table of maximal profit we can achive at time step i
    table = array bnds [(i, profit i) | i <- range bnds]
    -- The maximum profit made at time step i is either:
    --   1. if the price is rising, the max profit made at (i-1) plus the 
    --      increase in price form (i-1) to i.
    --   2. if the price is falling, the max profit made at (i-1).
    profit 0 = 0
    profit i
      | 0 < diff  = table!(i-1) + diff
      | otherwise = table!(i-1)
        where
          diff = prices!i - prices!(i-1)
