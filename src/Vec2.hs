module Vec2 where

data Vec2 = Vec2 !Double !Double

infixl 7 *:
infixl 6 +:
(+:), (-:) :: Vec2 -> Vec2 -> Vec2
Vec2 x1 y1 +: Vec2 x2 y2 = Vec2 (x1+x2) (y1+y2)
Vec2 x1 y1 -: Vec2 x2 y2 = Vec2 (x1-x2) (y1-y2)

(*:) :: Double -> Vec2 -> Vec2
a *: Vec2 x1 y1 = Vec2 (a*x1) (a*y1)

rotateVec :: Double -> Vec2 -> Vec2
rotateVec angle (Vec2 x y) = Vec2 (x * cos angle - y * sin angle)
                                  (x * sin angle + y * cos angle)

rotateVecAround :: Vec2 -> Double -> Vec2 -> Vec2
rotateVecAround pivot angle vec = rotateVec angle (vec -: pivot) +: pivot
