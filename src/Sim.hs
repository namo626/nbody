
module Sim where

import Data.List (zipWith4)
import Data.List.Split
import Control.Parallel.Strategies
import Types


g :: Double
g = 6.674e-11

stepSize = 1000


-- | Calculate the gravitational force vector acted on p1 by p2
getForce :: Particle -> Particle -> Vec
getForce p1 p2
  | p1 == p2  = zero
  | otherwise = (g * (m1 * m2 / r^2)) *^ unit
  where
    m1 = mass p1
    m2 = mass p2
    r1 = position p1
    r2 = position p2
    dr = r2 - r1
    r  = norm dr
    unit = dr ^/ r

-- | Calculate the net gravitational force acted on the particle by
-- all the other particles
getForces :: Particle -> [Particle] -> Vec
getForces p ps = sum $ map (getForce p) ps
--getForces p ps = foldr ((+) . getForce p) 0 ps


-- | Update the position and velocity of a particle in the system
-- through a given timestep
move :: Double -> System -> Particle -> Particle
move h sys p = let
  (pos, vel) = (position p, velocity p)
  acc        = (getForces p $ particles sys) ^/ mass p
  in
    p { position = pos + h *^ vel
      , velocity = vel + h *^ acc
      }


-- | Update the system by one timestep
evolve :: System -> System
evolve sys = sys { particles = ps' }
  where
    ps' = map (move stepSize sys) (particles sys) `using` parListChunk s rdeepseq
    s = number sys `quot` (4 * 4)


-- | Stream of system at different timesteps
evolution :: System -> [System]
evolution = iterate evolve
