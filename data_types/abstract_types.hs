module AbstractTypes where

import           Data.Char       (isDigit)
import           Data.List       (find)
import           Data.List.Split (splitOn)

data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance = undefined
-- distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

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


-- ************************************************* --

findDigit :: String -> Maybe Char
findDigit = find isDigit

findDigitOrX :: String -> Char
findDigitOrX s =
  case findDigit s of
    Nothing -> 'X'
    Just n  -> n


-- ************************************************* --

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x


-- ************************************************* --

data Error
    = ParsingError
    | IncompleteDataError
    | IncorrectDataError String
    deriving Show

data Person = Person
    { firstName :: String
    , lastName  :: String
    , age       :: Int
    } deriving Show

parseField :: String -> Either Error String
parseField s =
  case splitOn " = " s of
    [key, value]
      -> if null key && null value
        then Left ParsingError
        else Right value
    _ -> Left ParsingError


parsePerson :: String -> Either Error Person
parsePerson s =
  case map parseField (splitOn "\n" s) of
    [Right fn, Right sn, Right a]
      -> if all isDigit a
        then Right (Person fn sn (read a))
        else Left (IncorrectDataError a)
    [_, _, _] -> Left ParsingError
    [_, _] -> Left IncompleteDataError
    [_] -> Left IncompleteDataError
    [] -> Left IncompleteDataError
