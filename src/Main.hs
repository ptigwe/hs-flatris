-- | Haskell language pragma
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Haskell module declaration
module Main where

import Control.Arrow
import qualified Data.Map.Lazy as M

-- | Miso framework import
import Miso
import qualified Miso.String as S

import Action
import Model
import View

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model = 0 -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Model Action
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do putStrLn "Hello World" >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel model =
  div_
    [id_ "root", style_ . M.fromList $ [("padding", "30px 0")]]
    [ div_
        [ class_ "game"
        , style_ . M.fromList . reverse $
          [ ("height", "680px")
          , ("margin", "auto")
          , ("position", "relative")
          , ("width", "480px")
          ]
        ]
        [renderView model]
    ]
