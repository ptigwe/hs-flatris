module Model where

import Miso
import Miso.String (MisoString)
import qualified Miso.String as S
import Tetromino

data Model = Model
  { linesCleared :: Int
  , state :: State
  , score :: Int
  , active :: [[Int]]
  , pos :: (Int, Int)
  , color :: MisoString
  } deriving (Show, Eq)

data State
  = Paused
  | Playing
  | Stopped
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
  { linesCleared = 0
  , state = Stopped
  , score = 0
  , active = tetroShape TShaped
  , pos = (0, 0)
  , color = tetroColor TShaped
  }
