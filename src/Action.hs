module Action where

import Miso
import Miso.String (MisoString)
import qualified Miso.String as S

data Action
  = Init
  | Load String
  | Start
  | Pause
  | Resume
  | UnlockButtons
  | MoveLeft Bool
  | MoveRight Bool
  | Rotate Bool
  | Accelerate Bool
  | GetArrows Arrows
  | Time Double
  | Noop
  deriving (Show, Eq)
