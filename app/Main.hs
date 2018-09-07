module Main where

import Visualize
import Sim
import Graphics.Gloss
import Numeric.LinearAlgebra

-- system1 = System
--   { number = 2
--   , particles =
--       [ Particle 1 (vector [-1000, 0, 0]) (vector [0, 0, 0]) 5.972e15
--       , Particle 2 (vector [1000, 0, 0]) (vector [0, 0, 0]) 5.972e15
--       ]
--   }

sys :: System
sys = mkSystem2D (1000, 5000) (-500, 700) (0, 5) 2

sys2 :: System
sys2 = System
  { number = 2
  , particles = [
      Particle 1 (vector [-500, 0]) (vector [0, 0]) 6e15,
      Particle 2 (vector [ 500, 0]) (vector [0, 0]) 6e15
      ]
  }

sysEvo :: [System]
sysEvo = evolution sys

main :: IO ()
main = do
  let evos = evolution sys2
      state = head evos
      pic   = render state
      next  = evos !! 150

  print state
  print $ next
  display window background (render next)
