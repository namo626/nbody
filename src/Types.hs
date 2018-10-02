module Types
  ( Particle
  , mkParticle
  , tag
  , position
  , velocity
  , mass
  , Vec
  , mkVector
  , norm
  , zero
  , (*^)
  , (^*)
  , (^/)
  , toList
  , System
  , mkSystem
  , number
  , particles
  )
  where


import qualified Linear.Vector as V
import qualified Linear.Metric as M
import Linear.V2
import Control.DeepSeq
import System.Random

type Vec = V2 Double

data Particle = Particle
  { tag      :: !Int
  , position :: !Vec
  , velocity :: !Vec
  , mass     :: !Double
  }

instance Eq Particle where
  p1 == p2 = (tag p1) == (tag p2)

instance Show Particle where
  show p = show $ position p

instance NFData Particle where
  rnf p = t `seq` ps `seq` vel `seq` m `seq` ()
    where
      t   = tag p
      ps  = position p
      vel = velocity p
      m   = mass p

data System = System
  { number    :: !Int
  , particles :: [Particle]
  }

instance Show System where
  show sys = show (particles sys)

instance NFData System where
  rnf sys = rnf $ particles sys


-- | Construct a vector from a list of elements
mkVector :: [Double] -> Vec
mkVector [x, y] = V2 x y

mkParticle :: Int -> Vec -> Vec -> Double -> Particle
mkParticle = Particle

-- | Construct a system from a given list of
-- position vectors, velocity vectors, and masses
mkSystem :: [(Vec, Vec, Double)] -> System
mkSystem xs = System
  { number = length xs
  , particles = map (apply Particle) $ zipWith cons [0..] xs
  }
  where
    cons y (a, b, c)     = (y, a, b, c)
    apply f (y, a, b, c) = f y a b c


-- abstract vector operations
zero :: Vec
zero = V.zero

(^+^) :: Vec -> Vec -> Vec
v1 ^+^ v2 = v1 V.^+^ v2

(^-^) :: Vec -> Vec -> Vec
v1 ^-^ v2 = v1 V.^-^ v2

(^*) :: Vec -> Double -> Vec
v1 ^* n = v1 V.^* n

(*^) :: Double -> Vec -> Vec
n *^ v1 = n V.*^ v1

(^/) :: Vec -> Double -> Vec
v ^/ n = v V.^/ n

sumV :: [Vec] -> Vec
sumV = V.sumV

norm :: Vec -> Double
norm = M.norm

toList :: Vec -> [Double]
toList (V2 x y) = [x, y]



-- randomParticles :: Int
--             -> (Double, Double)
--             -> (Double, Double)
--             -> (Double, Double)
--             -> Int
--             -> [Particle]
-- randomParticles dim mr pr vr n =
--   let gen = mkStdGen 0
--       ms = randomRs mr gen
--       ps = groupV $ randomRs pr gen
--       vs = groupV $ randomRs vr gen
--       ns = [0..n-1]
--       groupV = map vector . chunksOf n
--   in
--     force $ zipWith4 Particle ns ps vs ms

-- randomSystem2D mr pr vr n = System
--   { number = n
--   , particles = randomParticles 2 mr pr vr n
--   }

-- randomSystem3D mr pr vr n = System
--   { number = n
--   , particles = randomParticles 3 mr pr vr n
--   }
