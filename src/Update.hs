{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Update where

import Control.Arrow
import Data.Function
import qualified Data.Map.Lazy as M
import Data.Monoid
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
  noEff model {arrows = (arrowX, arrowY)}
updateModel (Time newTime) model = step newModel
  where
    newModel = model {delta = newTime - time model, time = newTime}
updateModel _ model@Model {..} = noEff model

step :: Model -> Effect Model Action
step model@Model {..} = k <# do Time <$> now
  where
    k =
      model & moveTetromino time & rotateTetromino time & dropTetromino time &
      checkEndGame

moveTetromino :: Double -> Model -> Model
moveTetromino time = id

rotateTetromino :: Double -> Model -> Model
rotateTetromino time = id

dropTetromino :: Double -> Model -> Model
dropTetromino time = id

checkEndGame :: Model -> Model
checkEndGame = id
