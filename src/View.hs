{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module View where

import Control.Arrow
import qualified Data.Map.Lazy as M
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S

import Action
import Model

default (MisoString)

shapeList :: Char -> [(MisoString, MisoString)]
shapeList 'Z' = [("0%", "0%"), ("0%", "25%"), ("25%", "25%"), ("25%", "50%")]

renderSquare :: (MisoString, MisoString) -> View Action
renderSquare (top, left) =
  li_
    [ class_ "grid-square-block"
    , style_ . M.fromList $
      [ ("top", top)
      , ("left", left)
      , ("width", "25%")
      , ("height", "25%")
      , ("position", "absolute")
      ]
    ]
    [ div_
        [ class_ "square-block"
        , style_ . M.fromList $
          [ ("background-color", "#ecf0f1")
          , ("width", "100%")
          , ("height", "100%")
          ]
        ]
        []
    ]

renderNext :: Model -> View Action
renderNext model =
  div_
    [ class_ "next-tetromino next-tetromino-Z"
    , style_ . M.fromList $
      [ ("padding", "0px")
      , ("margin-top", "8px")
      , ("overflow", "hidden")
      , ("position", "relative")
      , ("width", "92px")
      , ("height", "92px")
      ]
    ]
    [ ul_
        [ class_ "tetromino"
        , style_ . M.fromList $
          [ ("margin", "0")
          , ("padding", "0")
          , ("list-style-type", "none")
          , ("position", "relative")
          , ("width", "100%")
          , ("height", "100%")
          ]
        ]
        (map renderSquare . shapeList $ 'Z')
    ]

renderWell :: Model -> View Action
renderWell model =
  div_
    [ class_ "well-container"
    , style_ . M.fromList $
      [ ("position", "absolute")
      , ("top", "0")
      , ("left", "0")
      , ("background", "#ecf0f1")
      , ("padding", "0px")
      , ("margin", "0px")
      , ("overflow", "hidden")
      , ("width", "300px")
      , ("height", "600px")
      ]
    ]
    []

renderControlButton :: MisoString -> View Action
renderControlButton x =
  div_
    [ style_ . M.fromList $
      [ ("background", "#ecf0f1")
      , ("border", "0")
      , ("color", "#34495f")
      , ("cursor", "pointer")
      , ("text-align", "center")
      , ("-webkit-user-select", "none")
      , ("display", "block")
      , ("float", "left")
      , ("font-family", "Helvetica, Arial, sans-serif")
      , ("font-size", "24px")
      , ("font-weight", "300")
      , ("height", "60px")
      , ("line-height", "60px")
      , ("margin", "20px 20px 0 0")
      , ("outline", "none")
      , ("padding", "0")
      , ("width", "60px")
      ]
    ]
    [text x]

renderControls :: View Action
renderControls =
  div_
    [ class_ "controls"
    , style_ . M.fromList $
      [("height", "8%"), ("left", "0"), ("position", "auto"), ("top", "600px")]
    ]
    (map renderControlButton ["↻", "←", "→", "↓"])

renderTitle :: MisoString -> View Action
renderTitle title =
  div_
    [ style_ . M.fromList $
      [ ("color", "#34495f")
      , ("font-size", "40px")
      , ("line-height", "60px")
      , ("margin", "30px 0 0")
      ]
    ]
    [text title]

renderLabel :: MisoString -> View Action
renderLabel label =
  div_
    [ style_ . M.fromList $
      [ ("color", "#bdc3c7")
      , ("font-weight", "300")
      , ("line-height", "1")
      , ("margin", "30px 0 0")
      ]
    ]
    [text label]

renderCount :: Int -> View Action
renderCount count =
  div_
    [ style_ . M.fromList $
      [ ("color", "#3993d0")
      , ("font-size", "30px")
      , ("line-height", "1")
      , ("margin", "5px 0 0")
      ]
    ]
    [text . S.toMisoString . show $ count]

renderGameButton :: MisoString -> View Action
renderGameButton state =
  button_
    [ style_ . M.fromList $
      [ ("background", "#34495f")
      , ("border", "0")
      , ("bottom", "30px")
      , ("color", "#fff")
      , ("cursor", "pointer")
      , ("display", "block")
      , ("font-family", "Helvetica, Arial, sans-serif")
      , ("font-size", "18px")
      , ("font-weight", "300")
      , ("height", "60px")
      , ("left", "30px")
      , ("line-height", "60px")
      , ("outline", "none")
      , ("padding", "0")
      , ("position", "auto")
      , ("width", "120px")
      ]
    ]
    [text state]

renderPanel :: Model -> View Action
renderPanel model =
  div_
    [ class_ "game-panel-container"
    , style_ . M.fromList $
      [ ("bottom", "80px")
      , ("color", "#34495f")
      , ("font-family", "Helvetica, Arial, sans-serif")
      , ("font-size", "14px")
      , ("left", "300px")
      , ("padding", "0 30px")
      , ("position", "absolute")
      , ("right", "0")
      , ("top", "0")
      ]
    ]
    [ renderTitle "Flatris"
    , renderLabel "Score"
    , renderCount 0
    , renderLabel "Lines Cleared"
    , renderCount 0
    , renderLabel "Next Shape"
    , div_
        [ class_ "next"
        , style_ . M.fromList $
          [("margin-top", "10px"), ("position", "relative")]
        ]
        [renderNext model]
    , renderGameButton "New Game"
    ]

renderInfo :: Model -> View Action
renderInfo model =
  div_
    [ class_ "info-panel-container"
    , style_ . M.fromList $
      [ ("background", "rgba(236, 240, 241, 0.85)")
      , ("color", "#34495f")
      , ("font-family", "Helvetica, Arial, sans-serif")
      , ("font-size", "18px")
      , ("height", "600px")
      , ("left", "0")
      , ("line-height", "1.5")
      , ("padding", "0 15px")
      , ("position", "absolute")
      , ("top", "0")
      , ("width", "270px")
      , ("display", "block")
      ]
    ]
    [ p_
        []
        [ text "hs-flatris is a "
        , b_ [] [text "Flatris "]
        , text "clone coded in Haskell using the Miso library "
        ]
    , p_
        []
        [ text "Inspired by the classic "
        , b_ [] [text "Tetris "]
        , text
            "game, the game can be played with a keyboard using the arrow keys, and on mobile devices using the buttons below."
        ]
    ]

renderView :: Model -> View Action
renderView model =
  div_
    [class_ "flatris-game"]
    [renderWell model, renderControls, renderPanel model, renderInfo model]
