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
updateModel (Time newTime) model = step newModel
  where
    newModel = model {delta = newTime - time model, time = newTime}
updateModel (GetArrows arr@Arrows {..}) model@Model {..} = step newModel
  where
    newModel = model {arrows = (arrowX, arrowY)}
updateModel Init model@Model {..} = model <# (Time <$> now)
updateModel _ model@Model {..} = noEff model

step :: Model -> Effect Model Action
step model@Model {..} = k <# (Time <$> now)
  where
    k =
      model & updateAnimation & moveTetromino & rotateTetromino &
      dropTetromino time &
      checkEndGame

updateAnimation :: Model -> Model
updateAnimation model@Model {..} =
  model {rotation = newRotation, movement = newMovement}
  where
    newRotation = updateAnimation_ time rotation
    newMovement = updateAnimation_ time movement

updateAnimation_ :: Double -> AnimationState -> AnimationState
updateAnimation_ time state@AnimationState {..} =
  state {ticks = newTicks, isAnimated = newAnimated}
  where
    newTicks = getTicks time delay
    newAnimated = newTicks /= ticks

getTicks :: Double -> Int -> Int
getTicks time delay = floor time `div` delay

moveTetromino :: Model -> Model
moveTetromino model@Model {..} = model {x = newX}
  where
    newX = move_ movement x (fst arrows)

move_ :: AnimationState -> Int -> Int -> Int
move_ state@AnimationState {..} x dir =
  if isAnimated
    then x + dir
    else x

rotateTetromino :: Model -> Model
rotateTetromino model@Model {..} = model {active = newActive}
  where
    newActive = rotate_ rotation active

rotate_ :: AnimationState -> [[Int]] -> [[Int]]
rotate_ state@AnimationState {..} tetro =
  case isAnimated of
    True -> rotate tetro
    False -> tetro

dropTetromino :: Double -> Model -> Model
dropTetromino time = id

checkEndGame :: Model -> Model
checkEndGame = id
