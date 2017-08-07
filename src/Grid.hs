{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Grid where

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
stamp width height sample grid = grid

collide :: Int -> Int -> Int -> Int -> Grid a -> Grid a -> Bool
collide width height x y sample grid = True

fullLine :: Int -> Grid a -> Maybe Int
fullLine width grid = Nothing

clearLines :: Int -> Grid a -> (Grid a, Int)
clearLines width grid = (grid, 0)
