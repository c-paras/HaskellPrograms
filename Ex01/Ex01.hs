{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Creates images of simple objects using the ShapeGraphics module.
-}

module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- line graphics programming interface
import ShapeGraphics

-- Part 1
-- writeToFile housePic
-- writeToFile chimneyHouse

-- picture of a house
housePic :: Picture
housePic = [door, house]
  where
    house = Path (convertToPoint houseCOs) green Solid
    door  = Path (convertToPoint doorCOs) red Solid

-- picture of a house with a chimney and smoke
chimneyHouse :: Picture
chimneyHouse = housePic ++ [chimney, smoke]
  where
    chimney = Path (convertToPoint chimneyCOs) green Solid
    smoke   = Path (convertToPoint smokeCOs) grey Solid

-- converts a list of pairs of Float to a list of Point
convertToPoint :: [(Float, Float)] -> [Point]
convertToPoint [] = []
convertToPoint ((x,y):xs) = [(Point x y)] ++ (convertToPoint xs)

-- coordinates of house
houseCOs :: [(Float, Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200), (730, 450), (700, 450), (700, 750)]

-- coordinates of door
doorCOs :: [(Float, Float)]
doorCOs = [(420, 750), (420, 550), (580, 550), (580, 750)]

-- coordinates of chimney
chimneyCOs :: [(Float, Float)]
chimneyCOs = [(615, 325), (615, 250), (650, 250), (650, 363)]

-- coordinates of smoke
smokeCOs :: [(Float, Float)]
smokeCOs = [(635, 240), (625, 230), (635, 220), (625, 210)]

-- since grey is not defined in ShapeGraphics
grey :: Colour
grey = Colour 255 255 255 128

-- Part 2
-- let myRed = Colour 255 0 0 180
-- let redCircle = Circle (Point 400 400) 100 myRed Solid SolidFill
-- writeToFile [redCircle, movePictureObject (Vector 100 100) redCircle]
-- let redEllipse = Ellipse (Point 400 400) 100 200 100 myRed Solid SolidFill
-- writeToFile [redEllipse, movePictureObject (Vector 100 100) redEllipse]
-- let redPath = Path [(Point 350 350), (Point 350 450), (Point 450 450), (Point 450 350), (Point 350 350)] myRed Solid
-- writeToFile [redPath, movePictureObject (Vector 100 100) redPath]
-- let redPolygon = Polygon [(Point 350 350), (Point 350 450), (Point 450 450), (Point 450 350), (Point 350 350)] myRed Solid SolidFill
-- writeToFile [redPolygon, movePictureObject (Vector 100 100) redPolygon]

-- shifts a point by a vector
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

-- shifts a list of points by a vector
movePoints :: [Point] -> Vector -> [Point]
movePoints [] vec = []
movePoints (x:xs) vec = (movePoint x vec) : movePoints xs vec

-- shifts a picture object by a vector
movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path points colour lineStyle)
  = Path (movePoints points vec) colour lineStyle

movePictureObject vec (Circle center radius colour lineStyle fillStyle)
  = Circle (movePoint center vec) radius colour lineStyle fillStyle

movePictureObject vec (Ellipse centerPO widthPO heightPO rotationPO colourPO lineStylePO fillStylePO)
  = Ellipse (movePoint centerPO vec) widthPO heightPO rotationPO colourPO lineStylePO fillStylePO

movePictureObject vec (Polygon pointsPO colourPO lineStylePO fillStylePO)
  = Polygon (movePoints pointsPO vec) colourPO lineStylePO fillStylePO

-- Part 3
-- let myRed = Colour 153 0 153 100
-- writeToFile (simpleCirclePic myRed 1)
-- writeToFile (simpleCirclePic myRed 5)
-- writeToFile (simpleCirclePic myRed 10)

-- generates the Picture consisting of overlapping Circle
-- [Circle (Point 400 400) 1 * (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) n * (400/n) col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n
  = createCirclePic col [Circle (Point 400 400) (400/n) col Solid SolidFill] n

-- generates list of overlapping Circle
createCirclePic :: Colour -> Picture -> Float -> Picture
createCirclePic col pic n
  = if fromIntegral (length pic) == n
    then pic
    else createCirclePic col (pic ++ [Circle (Point 400 400) ((fromIntegral (length pic)) * (400/n)) col Solid SolidFill]) n

-- writes a picture to file "ex01.png" for testing
writeToFile pic = writePng "ex01.png" (drawPicture 3 pic)
