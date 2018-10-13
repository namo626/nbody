module Main where

--import Visualize
import qualified Sim as S
import qualified BarnesHut as BH
import Types
import Graphics.Gloss
import System.Environment
import Control.DeepSeq
--import Parser


-- sys :: System
-- sys = randomSystem2D (6e15, 7e15) (-500, 500) (0, 5) 500

sys1 :: System
sys1 = mkSystem 2.3e11
    [ (mkVector [1.496e11, 0], mkVector [0, 2.98e4], 5.974e24)
    , (mkVector [2.279e11, 0], mkVector [0, 2.41e4], 6.42e23)
    , (mkVector [5.79e10, 0], mkVector [0, 4.8e4], 3.3e23)
    , (mkVector [0, 0], mkVector [0, 0], 1.98e30)
    , (mkVector [ 1.08e11, 0], mkVector [0, 3.5e4], 4.87e24)
    ]

sys2 :: System
sys2 = mkSystem 500
  [ (mkVector [-500, 0], mkVector [0, 60], 6e15)
  , (mkVector [ 500, 0], mkVector [0, -60], 6e15)
  , (mkVector [ 0, 300], mkVector [0, 0], 6e15)
  ]

fps = 60
dt = 1

-- | Measure performance using dummy system
test :: IO ()
test = do
  [n, choice] <- fmap (map read) getArgs :: IO [Int]
  case choice of
    0 -> print $ particles $ loop n S.allForces sys1
    1 -> print $ particles $ loop n BH.allForces sys1


  --finalState `deepseq` (return ())
loop n alg sys
    | n == 0    = sys
    | otherwise = loop (n-1) alg (force $ evolve dt BH.allForces sys)


-- | 2D visualization
-- runSim :: IO ()
-- runSim = do
--   ls <- lines `fmap` readFile "/home/namo/Programs/nbody/planets.txt"

--   let radius    = read $ head ls :: Double
--       initState = toSystem $ tail ls

--   simulate window background fps initState render nextFrame
  -- mapM_ print (take 10 $ evolution initState)

main = test
