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
    initialAction = Init -- initial action to be executed on application load
    model = initialModel -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [arrowsSub GetArrows] -- empty subscription list

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Model Action
updateModel (GetArrows arrs@Arrows {..}) model@Model {..} =
  noEff model {pos = ((+ arrowX) *** (+ arrowY)) pos}
updateModel Resume model@Model {..} = noEff model {state = Playing}
updateModel Start model@Model {..} = noEff model {state = Playing}
updateModel Pause model@Model {..} = noEff model {state = Paused}
updateModel _ model@Model {..} = noEff model

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel model =
  div_
    [ onMouseUp UnlockButtons
    , id_ "root"
    , style_ . M.fromList $ [("padding", "30px 0")]
    ]
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
    , div_ [] [pre_ [] [text . S.toMisoString . show $ model]]
    ]
