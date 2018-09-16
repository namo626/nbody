
module Main where

import Visualize
import Sim
import Graphics.Gloss
import System.Environment
import Control.DeepSeq
import Parser


sys :: System
sys = randomSystem2D (6e15, 7e15) (-500, 500) (0, 5) 50

sys2 :: System
sys2 = mkSystem
  [ (mkVector [-500, 0], mkVector [0, 60], 6e15)
  , (mkVector [ 500, 0], mkVector [0, -60], 6e15)
  , (mkVector [ 0, 300], mkVector [0, 0], 6e15)
  ]

fps = 60

-- | Measure performance using dummy system
test :: IO ()
test = do
  [n] <- fmap (map read) getArgs :: IO [Int]
  let initState = sys
      states = evolution initState
      finalState = states !! n

  finalState `deepseq` (return ())

-- | 2D visualization
main :: IO ()
main = do
  ls <- lines `fmap` readFile "/home/namo/Programs/nbody/planets.txt"

  let radius     = read $ head ls :: Double
      initState  = toSystem $ tail ls

  simulate window background fps initState render nextFrame
  -- mapM_ print (take 10 $ evolution initState)
