{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Grid
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S
import System.Random
import Tetromino

data Model = Model
  { linesCleared :: Int
  , state :: State
  , score :: Int
  , active :: [[Int]]
  , x :: Int
  , y :: Int
  , color :: MisoString
  , arrows :: (Int, Int)
  , rotation :: AnimationState
  , movement :: AnimationState
  , fall :: AnimationState
  , time :: Double
  , delta :: Double
  , width :: Int
  , height :: Int
  , grid :: Grid MisoString
  , nextTetro :: Tetromino
  , randSeed :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AnimationState = AnimationState
  { isAnimated :: Bool -- Whether or not to perform the action
  , isActive :: Bool -- Whether or not it is the currect time to act
  , ticks :: Int
  , delay :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data State
  = Paused
  | Playing
  | Stopped
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

initialModel :: Model
initialModel =
  Model
  { linesCleared = 0
  , state = Stopped
  , score = 0
  , active = tetroShape IShaped
  , x = 0
  , y = 0
  , color = tetroColor IShaped
  , arrows = (0, 0)
  , rotation = defaultMovement
  , movement = defaultMovement
  , time = 0
  , delta = 0
  , fall = defaultFall
  , width = 10
  , height = 20
  , grid = []
  , nextTetro = TShaped
  , randSeed = 0
  }

activeGrid :: [[Int]] -> MisoString -> Grid MisoString
activeGrid active color = fromList color . shapeToCoord $ active

defaultMovement :: AnimationState
defaultMovement =
  AnimationState {isAnimated = False, isActive = False, ticks = 0, delay = 10}

defaultFall :: AnimationState
defaultFall =
  AnimationState {isAnimated = False, isActive = False, ticks = 0, delay = 500}
