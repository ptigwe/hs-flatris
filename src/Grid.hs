{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Grid where

import Control.Arrow
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data Cell a = Cell
  { value :: a
  , pos :: (Int, Int)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Grid a = [Cell a]

fromList :: a -> [(Int, Int)] -> Grid a
fromList value = map (Cell value)

mapToList :: (a -> (Int, Int) -> b) -> Grid a -> [b]
mapToList fun = map (\cell@Cell {..} -> fun value pos)

emptyGrid :: Grid a
emptyGrid = []

stamp :: Int -> Int -> Grid a -> Grid a -> Grid a
stamp _ _ [] grid = grid
stamp x y (c:cs) grid = stamp x y cs (c {pos = newPos} : grid)
  where
    newPos = ((+ y) *** (+ x)) . pos $ c

collide :: Int -> Int -> Int -> Int -> Grid a -> Grid a -> Bool
collide width height x y sample grid =
  case sample of
    [] -> False
    (c:cs) ->
      let (y_, x_) = ((+ y) *** (+ x)) . pos $ c
      in (x_ >= width ||
          x_ < 0 || y_ >= height || (elem (y_, x_) . map pos $ grid)) ||
         collide width height x y cs grid

partFun :: (a -> Bool) -> [a] -> ([a], [a])
partFun f lst = (filter f lst, filter (not . f) lst)

fullLine :: Int -> Grid a -> Maybe Int
fullLine width [] = Nothing
fullLine width grid@(c:_) =
  if length inline == width
    then Just lineY
    else fullLine width remaining
  where
    lineY = fst . pos $ c
    (inline, remaining) = partFun (\c' -> lineY == (fst . pos $ c')) grid

clearLines :: Int -> Grid a -> (Grid a, Int)
clearLines width grid =
  case fullLine width grid of
    Nothing -> (grid, 0)
    Just lineY -> clearLine width lineY grid

clearLine :: Int -> Int -> Grid a -> (Grid a, Int)
clearLine width lineY grid = (newGrid, lines + 1)
  where
    clearedGrid = filter (\c' -> lineY /= (fst . pos $ c')) grid
    (above, below) = partFun (\c' -> lineY > (fst . pos $ c')) clearedGrid
    droppedAbove =
      map (\c' -> c' {pos = ((+ 1) . fst . pos $ c', snd . pos $ c')}) above
    (newGrid, lines) = clearLines width (droppedAbove ++ below)
