module Draw where



import Graphics.Rendering.Cairo

import Triangle
import Vec2



setColorHexA :: Integer -> Integer -> Render ()
setColorHexA hex a
  = let (rg, b) = quotRem hex 256
        (r, g) = quotRem rg 256
    in setSourceRGBA (fromIntegral r / 256)
                     (fromIntegral g / 256)
                     (fromIntegral b / 256)
                     (fromIntegral a)

setColorHex :: Integer -> Render ()
setColorHex hex = setColorHexA hex 1

drawTriangle :: Triangle -> Render ()
drawTriangle (Triangle shape (Vec2 x1 y1)  (Vec2 x2 y2)  (Vec2 x3 y3)) = do
    let color = case shape of
            Narrow -> 0x3e152e
            Wide   -> 0x6f2552

    do
        newPath
        setColorHex color
        moveTo x1 y1
        lineTo x2 y2
        lineTo x3 y3
        fill
        closePath

    do
        newPath
        setColorHex color
        moveTo x2 y2
        lineTo x3 y3
        setLineWidth 2
        stroke
        closePath

    do
        newPath
        setSourceRGBA 0 0 0 1
        setLineCap LineCapRound
        moveTo x2 y2
        lineTo x1 y1
        lineTo x3 y3
        setLineWidth 2
        stroke
        closePath

narrowTriangle :: Triangle
narrowTriangle = Triangle Narrow a b c
  where
    a = Vec2 0 0
    b = rotateVec (  deg2rad 18) (Vec2 1 0)
    c = rotateVec (- deg2rad 18) (Vec2 1 0)

deg2rad :: Double -> Double
deg2rad deg = deg * 2 * pi / 360

wideTriangle :: Triangle
wideTriangle = Triangle Wide a b c
  where
    a = Vec2 0 0
    b = rotateVec (  deg2rad 54) (Vec2 1 0)
    c = rotateVec (- deg2rad 54) (Vec2 1 0)

wideRhombus :: [Triangle]
wideRhombus = [ wideTriangle, moveTriangle (Vec2 (2 * cos (deg2rad 54)) 0) (mirrorTriangleY wideTriangle) ]

decagon :: [Triangle]
decagon
  = let tris = replicate 10 narrowTriangle
        tris' = zipWith ($) (cycle [id, mirrorTriangleX]) tris
        trisAngled = zipWith (rotateTriangle (Vec2 0 0)) [2*pi/10*i | i <- [0..10]] tris'
    in fitIntoBox (1000,800) trisAngled

initialTriangles :: [Triangle]
initialTriangles = fitIntoBox (1000,800) decagon

fitIntoBox :: (Integer, Integer) -> [Triangle] -> [Triangle]
fitIntoBox (boxWidth, boxHeight) tris = map (scaleTriangle minScale . moveTriangle (Vec2 0 0 -: Vec2 xMin yMin)) tris
  where
    (xss, yss) = unzip [ ([x1, x2, x3], [y1, y2, y3]) | Triangle _ty (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3) <- tris ]
    (xs, ys) = (concat xss, concat yss)
    (xMax, yMax) = (maximum xs, maximum ys)
    (xMin, yMin) = (minimum xs, minimum ys)
    (picWidth, picHeight) = (xMax - xMin, yMax - yMin)
    xScale = fromIntegral boxWidth / picWidth
    yScale = fromIntegral boxHeight / picHeight
    minScale = min xScale yScale
