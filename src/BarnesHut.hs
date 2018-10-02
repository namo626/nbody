module BarnesHut where

import Types (Vec, mkVector, Particle, mkParticle, position, mass, velocity, (*^), toList, zero)
import Data.List (partition)

data Tree = Nil
          | Leaf Particle
          | Node Info Tree Tree Tree Tree
          deriving Show

data Info = Info { com :: Vec, totalMass :: Double }
  deriving Show

data Box = Box { xpos :: Double, ypos :: Double, size :: Double }

moveBox :: Box -> Double -> Double -> Box
moveBox box dx dy = box
  { xpos = xpos box + dx
  , ypos = ypos box + dy
  , size = (size box) / 2
  }

combineInfo :: [Tree] -> Info
combineInfo ts = Info center totalM
  where
    center       = (1/totalM) *^ (sum $ map combine ts)
    totalM       = sum $ map (totalMass . getInfo) ts
    combine tree = let info = getInfo tree in
                     (totalMass info) *^ (com info)


build :: Box -> [Particle] -> Tree
build _ []   = Nil
build _ [p]  = Leaf p
build box ps = Node info t1 t2 t3 t4
  where
    t1 = build (moveBox box (-half)  half)   ps1
    t2 = build (moveBox box   half   half)   ps2
    t3 = build (moveBox box (-half) (-half)) ps3
    t4 = build (moveBox box   half  (-half)) ps4

    (ps1, lp1) = partition (inRegion NW box) ps
    (ps2, lp2) = partition (inRegion NE box) lp1
    (ps3, lp3) = partition (inRegion SW box) lp2
    (ps4, _)   = partition (inRegion SE box) lp3

    info = combineInfo [t1, t2, t3, t4]
    half = (size box) / 2

data Quadrant = NW | NE | SW | SE deriving Eq

getInfo :: Tree -> Info
getInfo Nil = Info { com = zero,
                     totalMass = 0 }
getInfo (Node info _ _ _ _) = info
getInfo (Leaf p) = Info { com = position p,
                          totalMass = mass p }

inRegion :: Quadrant -> Box -> Particle -> Bool
inRegion qd box p
  | qd == NW = x < xp && y > yp
  | qd == NE = x > xp && y > yp
  | qd == SW = x < xp && y < yp
  | qd == SE = x > xp && y < yp
  where
    [x, y] = toList $ position p
    xp = xpos box
    yp = ypos box


-- testing
p1 = mkParticle 0 (mkVector [-10, 10]) zero 10
p2 = mkParticle 1 (mkVector [5, 12]) zero 10
p3 = mkParticle 2 (mkVector [12, 5]) zero 6
box = Box 0 0 20

tree = build box [p1, p2, p3]
