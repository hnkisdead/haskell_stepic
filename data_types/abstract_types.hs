module AbstractTypes where

data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) +  abs (y2 - y1)

-- ************************************************* --

getCenter :: Double -> Coord Int -> Coord Double
getCenter cellSize (Coord cellX cellY) = Coord cellCenterX cellCenterY
  where cellCenterX = fromIntegral cellX * cellSize + cellSize / 2
        cellCenterY = fromIntegral cellY * cellSize + cellSize / 2

getCell :: Double -> Coord Double -> Coord Int
getCell cellSize (Coord pointX pointY) = Coord cellX cellY
  where cellX = floor $ pointX / cellSize
        cellY = floor $ pointY / cellSize
