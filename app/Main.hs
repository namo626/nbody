
module Main where

import Visualize
import Sim
import Graphics.Gloss
import System.Environment
--import Control.DeepSeq


sys :: System
sys = randomSystem2D (6e15, 7e15) (-500, 500) (0, 5) 10

sys2 :: System
sys2 = mkSystem
  [ (mkVector [-500, 0], mkVector [0, 60], 6e15)
  , (mkVector [ 500, 0], mkVector [0, -60], 6e15)
  , (mkVector [ 0, 300], mkVector [0, 0], 6e15)
  ]

initState = sys2
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
