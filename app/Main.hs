module Main where

import Visualize
import Sim
import Graphics.Gloss

-- system1 = System
--   { number = 2
--   , particles =
--       [ Particle 1 (vector [-1000, 0, 0]) (vector [0, 0, 0]) 5.972e15
--       , Particle 2 (vector [1000, 0, 0]) (vector [0, 0, 0]) 5.972e15
--       ]
--   }

sys2 :: System
sys2 = mkSystem (5, 10) (100, 200) (0, 5) 2

evs :: [System]
evs = evolution sys2

main :: IO ()
main = do
  let state = head evs
      pic = render state

  print state
  display window background pic
