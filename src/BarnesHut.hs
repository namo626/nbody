module BarnesHut
  (
    allForces
  ) where

import Types
import Sim (force, getForce)
import Data.List (partition)

data Tree = Nil
          | Leaf !Particle
          | Node !Info !Tree !Tree !Tree !Tree
          deriving Show

data Info = Info
  { centerMass :: !Vec
  , totalMass  :: !Double
  , boxSize    :: !Double
  }
  deriving Show

getInfo :: Tree -> Info
getInfo Nil = Info { centerMass = zero, totalMass = 0, boxSize = 0 }
getInfo (Node info _ _ _ _) = info
getInfo (Leaf p) = Info { centerMass = position p, totalMass = mass p, boxSize = 0 }

data Box = Box
  { xpos :: !Double
  , ypos :: !Double
  , size :: !Double }

moveBox :: Box -> Double -> Double -> Box
moveBox box dx dy = box
  { xpos = xpos box + dx
  , ypos = ypos box + dy
  , size = (size box) / 2
  }

data Quadrant = NW | NE | SW | SE deriving (Eq, Show)


combineInfo :: Double -> [Tree] -> Info
combineInfo regionSize ts = Info center totalM regionSize
  where
    center       = (1/totalM) *^ (sum $ map combine ts)
    totalM       = sum $ map (totalMass . getInfo) ts
    combine tree = let info = getInfo tree in
                     (totalMass info) *^ (centerMass info)


build :: Box -> [Particle] -> Tree
build _ []   = Nil
build _ [p]  = Leaf p
build box ps = Node info t1 t2 t3 t4
  where
    t1 = build (moveBox box (-half)  half)   ps1
    t2 = build (moveBox box   half   half)   ps2
    t3 = build (moveBox box (-half) (-half)) ps3
    t4 = build (moveBox box   half  (-half)) ps4

    (ps1, lp1) = partition (inQuadrant NW box) ps
    (ps2, lp2) = partition (inQuadrant NE box) lp1
    (ps3, lp3) = partition (inQuadrant SW box) lp2
    (ps4, _)   = partition (inQuadrant SE box) lp3

    info = combineInfo regionSize [t1, t2, t3, t4]
    half = (size box) / 2
    regionSize = (size box) * 2


inQuadrant :: Quadrant -> Box -> Particle -> Bool
inQuadrant qd box p
  | qd == NW = x < xp && y > yp
  | qd == NE = x > xp && y > yp
  | qd == SW = x < xp && y < yp
  | qd == SE = x > xp && y < yp
  where
    [x, y] = toList $ position p
    xp = xpos box
    yp = ypos box


-- | Calculate the net force on a given particle by traversing
-- the Barnes-Hut tree at a particular moment
netForce :: Particle -> Tree -> Vec
netForce p1 Nil = zero
netForce p1 (Leaf p2) = getForce p1 p2
netForce p1 (Node info t1 t2 t3 t4)
  | farEnough p1 info = force (m1, r1) (m2, r2)
  | otherwise         = sum $ map (netForce p1) [t1, t2, t3, t4]
  where
    (m1, r1) = (mass p1, position p1)
    (m2, r2) = (totalMass info, centerMass info)

farEnough :: Particle -> Info -> Bool
farEnough p info = (s/d) < theta
  where
    s     = boxSize info
    d     = norm (r1 - r2)
    r1    = position p
    r2    = centerMass info
    theta = 0.1


-- | Calculate the net force of particle from a given list of particles
allForces :: System -> [Vec]
allForces sys = map (\p -> netForce p tree) ps
  where
    ps = particles sys
    tree = build (Box 0 0 (universe sys)) ps

-- | Parallel version of allForces
parAllForces :: System -> [Vec]
parAllForces = allForces


-- testing
p1 = mkParticle 0 (mkVector [-10, 10]) zero 10
p2 = mkParticle 1 (mkVector [5, 12]) zero 10
p3 = mkParticle 2 (mkVector [12, 5]) zero 6
box = Box 0 0 20
tree = build box [p1, p2, p3]

sys = mkSystem 20
               [ (mkVector [-10, 10], zero, 10e5)
               , (mkVector [10, -5], zero, 12e5)
               ]
