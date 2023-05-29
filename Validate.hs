module Validate where

import Game
import Ship ( Coordinates, Ship(coordinates) )
import Board (Cell (Hit, Empty, Miss))

isValidCoordinates :: Coordinates -> Bool
isValidCoordinates coords
  | fst coords >= 0
    && snd coords >= 0
    && fst coords <= maxSize
    && snd coords <= maxSize  = True
  | otherwise                 = False
  where maxSize = boardSize - 1

isValidShipCoordinates :: (Coordinates, Coordinates) -> Int -> [Ship] -> Bool
isValidShipCoordinates shipCoords size currentShips
  | not (isValidCoordinates (fst shipCoords))
    || not (isValidCoordinates (snd shipCoords))                    = False
  | not $ isShipHorizontal || isShipVertical                        = False
  | isShipHorizontal && horizontalDiff shipCoords /= size - 1       = False
  | isShipVertical && verticalDiff shipCoords /= size - 1           = False
  | or [isRangeOverlapping shipCoords x | x <- currentShipsCoords]  = False
  | otherwise                                                       = True
  where isShipHorizontal          = isRangeHorizontal shipCoords
        isShipVertical            = isRangeVertical shipCoords
        currentShipsCoords        = map coordinates currentShips

isValidCoordinatesRange :: (Coordinates, Coordinates) -> Bool
isValidCoordinatesRange coords
  | not (isValidCoordinates (fst coords))
    || not (isValidCoordinates (snd coords))                  = False
  | not $ isRangeHorizontal coords || isRangeVertical coords  = False
  | isRangeHorizontal coords && horizontalDiff coords <= 0    = False
  | isRangeVertical coords && verticalDiff coords <= 0        = False
  | otherwise                                                 = True

horizontalDiff :: (Coordinates, Coordinates) -> Int
horizontalDiff coords = snd (snd coords) - snd (fst coords)

verticalDiff :: (Coordinates, Coordinates) -> Int
verticalDiff coords = fst (snd coords) - fst (fst coords)

isRangeHorizontal :: (Coordinates, Coordinates) -> Bool
isRangeHorizontal (x,y)
  | fst x == fst y  = True
  | otherwise       = False

isRangeVertical :: (Coordinates, Coordinates) -> Bool
isRangeVertical (x,y)
  | snd x == snd y  = True
  | otherwise       = False

isRangeOverlapping :: (Coordinates, Coordinates) -> (Coordinates, Coordinates) -> Bool
isRangeOverlapping ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  x1 <= x4 && x2 >= x3 && y1 <= y4 && y2 >= y3

isEmptyCell :: Cell -> Bool
isEmptyCell Empty = True
isEmptyCell _ = False

isHitCell :: Cell -> Bool
isHitCell Hit = True
isHitCell _ = False

isMissCell :: Cell -> Bool
isMissCell Miss = True
isMissCell _ = False