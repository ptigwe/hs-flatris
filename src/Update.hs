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
import Grid
import Miso
import qualified Miso.String as S

import Action
import Model
import Tetromino

foreign import javascript unsafe "$r = performance.now();" now ::
               IO Double

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Model Action
updateModel Resume model@Model {..} =
  noEff model {state = Playing, fall = newFall}
  where
    newFall = fall {isActive = True}
updateModel Start model@Model {..} =
  noEff model {state = Playing, fall = newFall}
  where
    newFall = fall {isActive = True}
updateModel Pause model@Model {..} =
  noEff model {state = Paused, fall = newFall}
  where
    newFall = fall {isActive = False}
updateModel Rotate model@Model {..} = noEff model {rotation = newRotation}
  where
    newRotation = rotation {isActive = True}
updateModel MoveLeft model@Model {..} =
  noEff model {movement = newMovement, arrows = newArrows}
  where
    newMovement = movement {isActive = True}
    newArrows = (\(x, y) -> (-1, y)) arrows
updateModel MoveRight model@Model {..} =
  noEff model {movement = newMovement, arrows = newArrows}
  where
    newMovement = movement {isActive = True}
    newArrows = (\(x, y) -> (1, y)) arrows
updateModel (Time newTime) model = step newModel
  where
    newModel = model {delta = newTime - time model, time = newTime}
updateModel (GetArrows arr@Arrows {..}) model@Model {..} = step newModel
  where
    newModel = model {arrows = (arrowX, arrowY)} & checkArrows
updateModel Init model@Model {..} = model <# (Time <$> now)
updateModel _ model@Model {..} = noEff model

checkArrows :: Model -> Model
checkArrows model@Model {..} =
  case state of
    Playing -> model & checkMovement & checkRotation & checkDrop
    _ -> model

checkMovement :: Model -> Model
checkMovement model@Model {..} = model {movement = newMovement}
  where
    newMovement = movement {isActive = isActive movement || fst arrows /= 0}

checkRotation :: Model -> Model
checkRotation model@Model {..} = model {rotation = newRotation}
  where
    newRotation = rotation {isActive = isActive rotation || snd arrows == 1}

checkDrop :: Model -> Model
checkDrop model@Model {..} = model {fall = newFall}
  where
    newDelay =
      if snd arrows == -1
        then 100
        else 1000
    newFall = fall {delay = newDelay}

step :: Model -> Effect Model Action
step model@Model {..} = k <# (Time <$> now)
  where
    k = shouldStep model

shouldStep :: Model -> Model
shouldStep model@Model {..} =
  case state of
    Playing -> k
    _ -> model
  where
    k =
      model & updateAnimation & moveTetromino & rotateTetromino & dropTetromino &
      checkEndGame

updateAnimation :: Model -> Model
updateAnimation model@Model {..} =
  model {rotation = newRotation, movement = newMovement, fall = newFall}
  where
    newRotation = updateAnimation_ time rotation
    newMovement = updateAnimation_ time movement
    newFall = updateAnimation_ time fall

updateAnimation_ :: Double -> AnimationState -> AnimationState
updateAnimation_ time state@AnimationState {..} =
  state {ticks = newTicks, isAnimated = newAnimated}
  where
    newTicks = getTicks time delay
    newAnimated = newTicks /= ticks

getTicks :: Double -> Int -> Int
getTicks time delay = floor time `div` delay

moveTetromino :: Model -> Model
moveTetromino model@Model {..} =
  if isAnimated movement && isActive movement
    then move_ model
    else model

move_ :: Model -> Model
move_ model@Model {..} = model {movement = newMovement, x = newX}
  where
    x_ = x + fst arrows
    newMovement = movement {isActive = False}
    newX =
      if collide width height x_ y (fromList "" . shapeToCoord $ active) grid
        then x
        else x_

rotateTetromino :: Model -> Model
rotateTetromino model@Model {..} =
  if isAnimated rotation && isActive rotation
    then shiftPosition [0, 1, -1, 2, -2] model
    else model

shiftPosition :: [Int] -> Model -> Model
shiftPosition [] model = model
shiftPosition (dx:dxs) model@Model {..} =
  if collide width height x_ y (activeGrid newActive color) grid
    then shiftPosition dxs model
    else model {x = x_, active = newActive, rotation = newRotation}
  where
    x_ = x + dx
    newActive = rotate active
    newRotation = rotation {isActive = False}

dropTetromino :: Model -> Model
dropTetromino model@Model {..} =
  if isAnimated fall && isActive fall
    then drop_ model
    else model

drop_ :: Model -> Model
drop_ model@Model {..} =
  if collide width height x y_ (activeGrid active color) grid
    then model {grid = stamp x y (activeGrid active color) grid} &
         spawnTetromino &
         clearLines_
    else model {y = y_}
  where
    y_ = y + 1

spawnTetromino :: Model -> Model
spawnTetromino model@Model {..} = model {x = width `div` 2, y = 0}

clearLines_ :: Model -> Model
clearLines_ = id

checkEndGame :: Model -> Model
checkEndGame = id
