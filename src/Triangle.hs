module Triangle where



import Vec2



data TriangleType = Narrow | Wide
data Triangle = Triangle TriangleType Vec2 Vec2 Vec2

moveTriangle :: Vec2 -> Triangle -> Triangle
moveTriangle offset (Triangle ty a b c) = Triangle ty (a +: offset) (b +: offset) (c +: offset)

scaleTriangle :: Double -> Triangle -> Triangle
scaleTriangle factor (Triangle ty a b c) = Triangle ty (factor *: a) (factor *: b) (factor *: c)

rotateTriangle :: Vec2 -> Double -> Triangle -> Triangle
rotateTriangle pivot angle (Triangle ty a b c)
  = Triangle ty (rotateVecAround pivot angle a)
                (rotateVecAround pivot angle b)
                (rotateVecAround pivot angle c)

mirrorTriangleX :: Triangle -> Triangle
mirrorTriangleX (Triangle ty (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3))
  = Triangle ty (Vec2 x1 (-y1)) (Vec2 x2 (-y2)) (Vec2 x3 (-y3))

mirrorTriangleY :: Triangle -> Triangle
mirrorTriangleY (Triangle ty (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3))
  = Triangle ty (Vec2 (-x1) y1) (Vec2 (-x2) y2) (Vec2 (-x3) y3)
