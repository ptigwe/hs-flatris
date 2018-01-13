-- | Haskell language pragma
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell module declaration
module Main where

import Control.Arrow
import qualified Data.Map.Lazy as M
import System.Random

-- | Miso framework import
import Miso
import qualified Miso.String as S

import Action
import Model
import Update
import View

-- | Entry point for a miso application
main :: IO ()
main = do
  t <- now
  gen <- getStdGen
  let (tetro, nGen) = random gen
  let seed = fst . random $ nGen :: Int
  let m = initialModel {time = t, nextTetro = tetro, randSeed = seed}
  startApp App {model = m, initialAction = Init, ..}
  where
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [arrowsSub GetArrows] -- empty subscription list
    mountPoint = Nothing
