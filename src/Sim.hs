
module Sim
  ( allForces
  , force
  , getForce
  ) where

import Data.List (zipWith4)
import Data.List.Split
import Control.Parallel.Strategies
import Types


g :: Double
g = 6.674e-11

stepSize = 1

force :: (Double, Vec) -> (Double, Vec) -> Vec
force (m1, r1) (m2, r2) = (g * m1 * m2 / r^2) *^ unit
  where
    r = norm dr
    unit = dr ^/ r
    dr = r2 - r1

-- | Calculate the gravitational force vector acted on p1 by p2
getForce :: Particle -> Particle -> Vec
getForce p1 p2
  | p1 == p2  = zero
  | otherwise = force (m1, r1) (m2, r2)
  where
    (m1, r1) = (mass p1, position p1)
    (m2, r2) = (mass p2, position p2)

-- | Calculate the net gravitational force acted on the particle by
-- all the other particles
allForces :: System -> [Vec]
allForces sys = map netForce ps
  where
    ps = particles sys
    netForce p = sum $ map (getForce p) ps
