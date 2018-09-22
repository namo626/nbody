module Visualize where

import Graphics.Gloss
import GHC.Float (double2Float)
import Sim
import Types
import Graphics.Gloss.Data.ViewPort (ViewPort)

window :: Display
window = InWindow "Sim" (200, 200) (10, 10)

background :: Color
background = black

drawing :: Picture
drawing = drawParticle 10

drawParticle :: Float -> Picture
drawParticle r = color yellow $ circleSolid r

nextFrame :: ViewPort -> Float -> System -> System
nextFrame _ _ sys = evolve sys

render :: System -> Picture
render sys = pictures objs
  where
    objs = map (\p -> translate (scale $ p !! 0) (scale $ p !! 1) drawing) ps
    ps = map (map double2Float . toList . position) $ particles sys
    scale n = n / 2e8



apply2 :: (a -> a -> b) -> [a] -> b
apply2 f [x, y] = f x y
apply2 _ _      = error "List"
