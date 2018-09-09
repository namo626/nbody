module Parser where

import Sim

tabular :: [String] -> [[Double]]
tabular = map (map read . init . words) . filter (not . null)

vectorize :: [Double] -> (Vec, Vec, Double)
vectorize [px, py, vx, vy, m] = ( mkVector [px, py]
                                , mkVector [vx, vy]
                                , m
                                )

toSystem :: [String] -> System
toSystem = mkSystem . map vectorize . tabular
