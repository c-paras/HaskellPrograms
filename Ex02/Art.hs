{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Creates a fractal tree with given geometry and color.
-}

module Art where

import Codec.Picture
import ShapeGraphics

-- creates the "pretty" image
art :: Picture
art = fracTree 50 170 (pi/2) 10 orange

-- makes a fractal tree (n is the depth of recursion)
-- parameterizable by size, shape, color and depth of recursion
fracTree :: Float -> Float -> Float -> Int -> Colour -> Picture
fracTree width height angle n col
  = fTree (Point (400 - width/2) 700) (Vector 0 (-height)) (Vector width 0) col n
  where
    fadeToBlue :: Colour -> Colour
    fadeToBlue (Colour r g b o) = Colour (max 0 (r - 15)) g (min 255 (b + 15)) o

    -- recursively creates a fracTree
    fTree :: Point -> Vector -> Vector -> Colour -> Int -> Picture
    fTree pos vec1 vec2 col n

      -- base case
      | n <= 1 = [Polygon [pos, movePoint vec1 pos,
                                movePoint vec2 $ movePoint vec1 pos,
                                movePoint vec2 pos]
                           col Solid SolidFill]

      -- recursive case
      | otherwise = fTree pos vec1 vec2 col (n - 1) ++
                    fTree (movePoint vec1 pos)
                          (scaleVector 0.7 $ rotateVector (0.5 * angle) vec1)
                          (scaleVector 0.7 $ rotateVector (0.5 * angle) vec2)
                          (fadeToBlue col) (n-1) ++
                    fTree (movePoint vec1 pos)
                          (scaleVector 0.7 $ rotateVector (-angle) vec1)
                          (scaleVector 0.7 $ rotateVector (-angle) vec2)
                          (fadeToBlue col) (n-1)

-- scales Vector by a factor
scaleVector :: Float -> Vector -> Vector
scaleVector fac (Vector x y)
  = Vector (fac * x) (fac * y)

-- rotates Vector by an angle
rotateVector :: Float -> Vector -> Vector
rotateVector alpha (Vector vx vy)
  = Vector (cos alpha * vx - sin alpha * vy)
           (sin alpha * vx + cos alpha * vy)

-- shifts Point by a Vector
movePoint :: Vector -> Point -> Point
movePoint (Vector xv yv) (Point xp yp)
  = Point (xv + xp) (yv + yp)

-- writes a picture to file "art.png" for testing
writeToFile pic = writePng "art.png" (drawPicture 3 pic)
