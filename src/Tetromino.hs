{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Tetromino where

import Data.Aeson
import Data.List
import GHC.Generics
import Grid
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S
import System.Random

default (MisoString)

data Tetromino
  = IShaped
  | OShaped
  | TShaped
  | JShaped
  | LShaped
  | SShaped
  | ZShaped
  deriving (Eq, Generic, FromJSON, ToJSON, Show, Enum, Bounded)

instance Random Tetromino where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)

tetroToChar :: Tetromino -> Char
tetroToChar IShaped = 'I'
tetroToChar OShaped = 'O'
tetroToChar TShaped = 'T'
tetroToChar JShaped = 'J'
tetroToChar LShaped = 'L'
tetroToChar SShaped = 'S'
tetroToChar ZShaped = 'Z'

tetroShape :: Tetromino -> [[Int]]
tetroShape IShaped = [[0, 0, 0, 0], [1, 1, 1, 1], [0, 0, 0, 0], [0, 0, 0, 0]]
tetroShape OShaped = [[1, 1], [1, 1]]
tetroShape TShaped = [[1, 1, 1], [0, 1, 0], [0, 0, 0]]
tetroShape JShaped = [[1, 0, 0], [1, 1, 1], [0, 0, 0]]
tetroShape LShaped = [[0, 0, 1], [1, 1, 1], [0, 0, 0]]
tetroShape SShaped = [[0, 1, 1], [1, 1, 0], [0, 0, 0]]
tetroShape ZShaped = [[1, 1, 0], [0, 1, 1], [0, 0, 0]]

tetroColor :: Tetromino -> MisoString
tetroColor IShaped = "#3cc7d6"
tetroColor OShaped = "#fbb414"
tetroColor TShaped = "#b04497"
tetroColor JShaped = "#3993d0"
tetroColor LShaped = "#ed652f"
tetroColor SShaped = "#95c43d"
tetroColor ZShaped = "#e84138"

rotate :: [[Int]] -> [[Int]]
rotate = Data.List.transpose . reverse

shapeToCoord :: [[Int]] -> [(Int, Int)]
shapeToCoord shape =
  [(x, y) | (x, row) <- enumerate shape, (y, t) <- enumerate row, t == 1]
  where
    enumerate = zip [0 ..]

tetroGrid :: Tetromino -> Grid MisoString
tetroGrid tetro =
  fromList (tetroColor tetro) . shapeToCoord . tetroShape $ tetro

randomTetro :: Int -> (Tetromino, Int)
randomTetro seed = (tetro, newSeed)
  where
    (tetro, stdGen) = random (mkStdGen seed)
    (newSeed, _) = random stdGen
