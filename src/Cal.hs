module Cal where

import Types

type Strategy = (System -> [Vec])


-- | Update a particle given its net force
update :: Double -> Particle -> Vec -> Particle
update dt p f = let
  (pos, vel) = (position p, velocity p)
  in
    p { position = pos + dt *^ vel
      , velocity = vel + dt *^ f
      }


evolve :: Double
       -> Strategy
       -> System
       -> System
evolve dt strat sys = sys { particles = ps' }
  where
    ps = particles sys
    ps' = zipWith (update dt) ps (strat sys)
