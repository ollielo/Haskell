-- Solution to Problem 6.10 in "Algorithm Design"

import Debug.Trace
import Data.Array

-- Brute-force solution to the supercomputer scheduling problem.
superduper :: (Ord a, Num a) => [a] -> [a] -> a
superduper [] _ = 0
superduper _ [] = 0
superduper as bs 
  -- We don't know which computer to start with at the beginning, try both ways 
  -- and see which one works out.
  = max (super as bs) (super bs as)
  where
    -- At each time step, we can either choose to stay on the computer or switch
    -- to the other computer.
    super xs ys = max (stay xs ys) (switch xs ys)
    -- If we choose to stay on the same computer, we can perform the amount of  
    -- computation in available on the computer in current time step.
    stay   [x] [y]       = x
    stay   (x:xs) (y:ys) = x + super xs ys
    -- If we choose to switch to the other computer, we can not perform any
    -- computation at current time step. 
    switch [x] [y]       = 0              -- switch at the last minute
    switch (x:xs) (y:ys) = super ys xs 

-- Dynamic programming solution to the scheduling problem.
-- At each time step, the job can be from the same supercomputer as previous
-- time step which comsume the computation capability of the computer at the
-- time step, or it can be migrating from the other supercomputer thus can
-- not perform any computation. The value of each state is the maximum amount 
-- of computation done so far.
--
-- State transition DAG
--     5       1       20      20
-- 0b ---> 1b ---> 2b ---> 3b ---> 4b
--     X       X       X       X
-- 0a ---> 1a ---> 2a ---> 3a ---> 4a
--    10       1       1       10
-- 
-- Each X represent migration and contribute 0 to the total computation.
scheduler as bs = max (sched ! (0, len)) (sched ! (1, len))
  where
    len  = length as
    bnds = ((0,0), (1, len))
    -- creating the computation capability array
    capability = listArray ((0,1), (1, len)) $ as ++ bs
    -- creating the optimal schedule array
    sched = array bnds [((s, t), f s t) | s <- [0, 1], t <- [0..len]]
    f _ 0 = 0                 -- return 0 for both computers at time 0.
    f s t = max stay switch
      where
        stay          = capability ! (s, t) + sched ! (s, t-1)
        switch 
          | s == 0    = sched ! (1, t -1)
          | otherwise = sched ! (0, t -1)

main :: IO ()
main = putStrLn $ show $ scheduler [5,1,20,20] [10,1,1,10]
