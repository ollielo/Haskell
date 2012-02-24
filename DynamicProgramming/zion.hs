-- Solutions to Problem 6.8 in "Algorithm Design"

import Debug.Trace
import Data.Array

--zion :: (Ord a, Num a) => [a] -> [a] -> a
-- zion charge@[] robot 
--   | trace ("zion " ++ show charge  ++ " " ++ show robot) False = undefined
--   | otherwise = 0
-- zion charge robot@[] 
--   | trace ("zion " ++ show charge  ++ " " ++ show robot) False = undefined
--   | otherwise  = 0
-- zion charge@(c:cs) robot@(r:rs) 
--   | trace ("zion " ++ show charge  ++ " " ++ show robot ++ ", k0 " ++ show k0 ++ ", k1 " ++ show k1) False = undefined
--   | otherwise = max k0 k1  
--     where
--       k0 = (min c r) + zion recharge rs -- shot
--       k1 = zion cs rs
--      recharge = charge

-- morpheus charges robots = neo charges robots
--   where
--     recharge = charges          -- recharge will be treated as constant in neo
--     neo charge@[] robot 
--       | trace ("neo " ++ show charge  ++ " " ++ show robot) False = undefined
--       | otherwise = 0
--     neo charge robot@[] 
--       | trace ("neo " ++ show charge  ++ " " ++ show robot) False = undefined
--       | otherwise  = 0
--     neo charge@(c:cs) robot@(r:rs) 
--       | trace ("neo " ++ show charge  ++ " " ++ show robot ++ ", k0 " ++ show k0 ++ ", k1 " ++ show k1) False = undefined
--       | otherwise = max k0 k1  
--         where
--           k0 = (min c r) + neo recharge rs -- shoot the EMP and recharge 
--           k1 = neo cs rs                   -- wait for another wave

morpheus charges robots = neo charges robots
  where
    recharge = charges      -- recharge will be treated as constant in neo
    neo [] _ = 0
    neo _ [] = 0
    neo (c:cs) (r:rs) = max red blue  
        where
          red  = (min c r) + neo recharge rs -- shoot the EMP and recharge 
          blue = neo cs rs                   -- wait for another wave

-- Similar to morepheus but return the maximun total number of kills and number
-- number of kill at each time step.
morpheus' charges robots = neo charges robots
  where
    recharge = charges      -- recharge will be treated as constant in neo
    neo [] _ = (0, [])
    neo _ [] = (0, [])
    neo (c:cs) (r:rs) = max red blue  
        where
          -- shoot the EMP and recharge
          red   = (kills + fst reset, kills: snd reset)
          kills = min c r
          reset = neo recharge rs
          -- wait for another wave
          blue  = (fst holdon, 0:snd holdon)
          holdon = neo cs rs

-- At each time step i, we can choose to wait j ==  to shoot the EMP  
-- Opt(0) = 0 
-- Opt(1) = max (Opt(0) + min(r1, c1))
--
-- Opt(2) = max (Opt(1) + min(r2, c1))   -- shot at t = 1 
--              (Opt(0) + min(r2, c2))   -- shot at t = 2
--
-- Opt(3) = max (Opt(2) + min(r3, c1))   -- shot at t = 2
--              (Opt(1) + min(r3, c2))   -- shot at t = 1
--              (Opt(0) + min(r3, c3))   -- shot at t = 0
--
-- Opt(4) = max (Opt(3) + min(r4, c1))   -- shot at t = 3
--              (Opt(2) + min(r4, c2))   -- shot at t = 2
--              (Opt(1) + min(r4, c3))   -- shot at t = 1
--              (Opt(0) + min(r4, c4))   -- shot at t = 0

trinity :: (Ord a, Num a) => [a] -> [a] -> Array Int a
trinity charges robots = max_kill
  where
    n   = length charges
    rbs = listArray (0, n) robots
    max_kill = array (0, n) [(i, holy max_kill i) | i <- [0..n]]
    holy _ 0 = 0
    holy a i = maximum $ zipWith (+) opts kills
      where
        opts  = map (a!) [i-1, i-2..0]
        kills = map (min (rbs!(i-1))) charges

