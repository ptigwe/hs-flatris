{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}

module Update where

import Control.Arrow
import qualified Data.Map.Lazy as M
import Miso
import qualified Miso.String as S

import Action
import Model

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Model Action
updateModel (GetArrows arrs@Arrows {..}) model@Model {..} =
  noEff model {pos = ((+ arrowX) *** (+ arrowY)) pos}
updateModel Resume model@Model {..} = noEff model {state = Playing}
updateModel Start model@Model {..} = noEff model {state = Playing}
updateModel Pause model@Model {..} = noEff model {state = Paused}
updateModel _ model@Model {..} = noEff model
