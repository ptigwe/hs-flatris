{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Update where

import Control.Arrow
import qualified Data.Map.Lazy as M
import Miso
import qualified Miso.String as S

import Action
import Model
import Tetromino

foreign import javascript unsafe "$r = performance.now();" now ::
               IO Double

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Model Action
updateModel Resume model@Model {..} = noEff model {state = Playing}
updateModel Start model@Model {..} = noEff model {state = Playing}
updateModel Pause model@Model {..} = noEff model {state = Paused}
updateModel Rotate model@Model {..} = noEff model {active = newactive}
  where
    newactive =
      case state of
        Playing -> rotate active
        _ -> active
updateModel (GetArrows arr@Arrows {..}) model@Model {..} =
  noEff model {arrows = arr}
updateModel _ model@Model {..} = noEff model
