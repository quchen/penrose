module Penrose (penroseIterations) where



import Triangle
import Vec2



subdivide :: Triangle -> [Triangle]
subdivide (Triangle ty tip left right) = case ty of
    Narrow ->
        let p = tip +: inverseGoldenRatio *: (left -: tip)
        in [ Triangle Narrow right p left
           , Triangle Wide p right tip ]
    Wide ->
        let q = left +: inverseGoldenRatio *: (tip -: left)
            p = left +: inverseGoldenRatio *: (right -: left)
        in [ Triangle Wide p right tip
           , Triangle Wide q p left
           , Triangle Narrow p q tip ]
  where
    inverseGoldenRatio :: Double
    inverseGoldenRatio = (sqrt 5 - 1) / 2

penroseIterations :: [Triangle] -> [[Triangle]]
penroseIterations = iterate (>>= subdivide)
