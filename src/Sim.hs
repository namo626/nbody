module Sim where

import Numeric.LinearAlgebra
import System.Random
import Data.List (zipWith4)
import Data.List.Split

type Vec = Vector Double

data Particle = Particle
  { tag      :: Int
  , position :: Vec
  , velocity :: Vec
  , mass     :: Double
  }

instance Eq Particle where
  p1 == p2 = (tag p1) == (tag p2)

instance Show Particle where
  show p = show $ position p

data System = System
  { number :: Int
  , particles :: [Particle]
  }

instance Show System where
  show sys = show (particles sys)

g :: Double
g = 6.674e-11

mkParticles :: (Double, Double)
            -> (Double, Double)
            -> (Double, Double)
            -> Int
            -> [Particle]
mkParticles mr pr vr n =
  let gen = mkStdGen 0
      ms = randomRs mr gen
      ps = groupV3 $ randomRs pr gen
      vs = groupV3 $ randomRs vr gen
      ns = [0..n-1]
      groupV3 = map vector . chunksOf 3
  in
    zipWith4 Particle ns ps vs ms

mkSystem mr pr vr n = System
  { number = n
  , particles = mkParticles mr pr vr n
  }

getForce :: Particle -> Particle -> Vec
getForce p1 p2 = scalar (g * (m1 * m2 / r^2)) * unit
  where
    m1 = mass p1
    m2 = mass p2
    r1 = position p1
    r2 = position p2
    dr = r2 - r1
    r = norm_2 dr
    unit = normalize dr

getForces :: Particle -> [Particle] -> Vec
getForces p ps = sum $ map (getForce p) ps

-- eulerStep :: (Double -> Double -> Double) -- f(t, y)
--           -> Double
--           -> (Double, Double) -- y
--           -> (Double, Double) -- value of next step
-- eulerStep f h (t0, y0) = (t + h, y + h*(f t y))

move :: Double -> System -> Particle -> Particle
move h sys p = let
  (pos, vel) = (position p, velocity p)
  others     = filter (/= p) (particles sys)
  acc        = (getForces p others) / (scalar $ mass p)

  in
    p { position = pos + scalar h * vel
      , velocity = vel + scalar h * acc
      }

stepSize = 0.1

evolve :: System -> System
evolve sys = sys { particles = ps' }
  where
    ps' = map (move stepSize sys) (particles sys)

evolution :: System -> [System]
evolution = iterate evolve
