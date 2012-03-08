import Data.Array

-- assuming graph is connected
shortestPath graph start terminal = table ! (start, nnodes)
  where
    nnodes = length graph
    bnds   = ((0, 0), (nnodes, nnodes))
    table  = array bnds [(ij, cost ij) | ij <- range bnds]
    -- the cost of pathes from terminal to terminal of any number of hops 
    -- is zero.
    -- TODO: a better to deal with Infinity?
    cost (_, 0) = 1/0
    cost (node, hops) 
      | node == terminal = 0
      | otherwise = min (table!(node, hops-1))
                    (minimum [c + table!(n, hops-1) | (n, c) <- graph!!node])

main :: IO ()
main =
  let graph = [[(1, 1), (3, 2), (4, 5)], 
               [(0, 1), (2, 2), (4, 5)],
               [(1, 2), (4, 1), (5, 4)],
               [(0, 2), (4, 3)],
               [(0, 5), (1, 5), (2, 1), (3, 3), (5, 1)],
               [(2, 4), (4, 1)]]
  in
      print $ shortestPath graph 0 5