module Parser where

import Sim
import Types

tabular :: [String] -> [[Double]]
tabular = map (map read . init . words) . filter (not . null)

vectorize :: [Double] -> (Vec, Vec, Double)
vectorize [px, py, vx, vy, m] = ( mkVector [px, py]
                                , mkVector [vx, vy]
                                , m
                                )

convertUnits :: [Double] -> [Double]
convertUnits [px, py, vx, vy, m] = [ astroLength px
                                   , astroLength py
                                   , astroLength vx
                                   , astroLength vy
                                   , astroMass m
                                   ]


toSystem :: [String] -> System
toSystem = mkSystem . map (vectorize) . tabular

-- | Solar mass
astroMass :: Double -> Double
astroMass m = m / 1.988e30

astroLength :: Double -> Double
astroLength l = l / 149597870700.0
