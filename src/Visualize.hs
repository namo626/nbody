module Visualize where

import Graphics.Gloss
import GHC.Float (double2Float)
import Numeric.LinearAlgebra
import Sim

window :: Display
window = InWindow "Sim" (200, 200) (10, 10)

background :: Color
background = black

drawing :: Picture
drawing = drawParticle 20

drawParticle :: Float -> Picture
drawParticle r = color yellow $ circleSolid r

render :: System -> Picture
render sys = pictures objs
  where
    objs = map (\p -> translate (p !! 0) (p !! 1) drawing) ps
    ps = map (map double2Float . toList . position) $ particles sys



apply2 :: (a -> a -> b) -> [a] -> b
apply2 f [x, y] = f x y
apply2 _ _      = error "List"
