module Visualize where

import Graphics.Gloss
import Sim

window :: Display
window = InWindow "Sim" (200, 200) (10, 10)

background :: Color
background = black

drawing :: Picture
drawing = circle 80

drawParticle :: Int -> Picture
drawParticle r = color yellow $ circleSolid r

render :: System -> Picture
render sys = pictures objs
  where
    objs = map (\p -> translate $$ p $ drawing) ps
    ps = map (toList . position) $ particles sys



($$) :: (a -> a -> b) -> [a] -> b
f $$ [x, y] = f x y
_ $$ _      = error "List"
