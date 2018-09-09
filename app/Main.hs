
module Main where

import Visualize
import Sim
import Graphics.Gloss
import Numeric.LinearAlgebra
import System.Environment
import Control.DeepSeq


-- system1 = System
--   { number = 2
--   , particles =
--       [ Particle 1 (vector [-1000, 0, 0]) (vector [0, 0, 0]) 5.972e15
--       , Particle 2 (vector [1000, 0, 0]) (vector [0, 0, 0]) 5.972e15
--       ]
--   }

sys :: System
sys = mkSystem2D (6e15, 7e15) (-500, 500) (0, 5) 10

sys2 :: System
sys2 = System
  { number = 2
  , particles = [
      Particle 1 (vector [-500, 0]) (vector [0, 60]) 6e15,
      Particle 2 (vector [ 500, 0]) (vector [0, -60]) 6e15,
      Particle 3 (vector [ 0, 300]) (vector [0, 0]) 6e15
      ]
  }

initState = sys
fps = 20

main :: IO ()
main = do
  -- simulate window background fps initState render nextFrame
  [n] <- fmap (map read) getArgs :: IO [Int]
  let states = take n $ evolution initState
  mapM_ print states

  --states `deepseq` (return ())
  -- let evos = evolution sys2
  --     state = head evos
  --     pic   = render state
  --     next  = evos !! 150

  -- print state
  -- print $ next
  -- display window background (render next)
