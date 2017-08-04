module Action where

import Miso
import qualified Miso.String as S

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)
