module Main (main) where



import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Penrose



main :: IO ()
main = do
    surface <- createImageSurface FormatARGB32 1000 800
    renderWith surface (do
        setSourceRGBA 0 0 0 0
        rectangle 0 0 1000 800
        fill
        let triangles = fitIntoBox (1000, 800) (penroseIterations initialTriangles !! 4)
        for_ triangles drawTriangle )
    surfaceWriteToPNG surface "out.png"
