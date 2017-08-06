{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S
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
  , time :: Double
  , delta :: Double
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
  , active = tetroShape TShaped
  , x = 0
  , y = 0
  , color = tetroColor TShaped
  , arrows = (0, 0)
  , rotation = defaultRotation
  , time = 0
  , delta = 0
  }

defaultRotation :: AnimationState
defaultRotation =
  AnimationState {isAnimated = False, isActive = False, ticks = 0, delay = 1000}
