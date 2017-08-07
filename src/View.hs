{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module View where

import Control.Arrow
import Data.Aeson.Encode.Pretty
import qualified Data.Map.Lazy as M
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S

import Action
import Model
import Tetromino

default (MisoString)

shapeList :: Char -> [(MisoString, MisoString)]
shapeList 'Z' = [("0%", "0%"), ("0%", "25%"), ("25%", "25%"), ("25%", "50%")]

renderSquare ::
     (MisoString, MisoString)
  -> ((MisoString, MisoString), MisoString)
  -> View Action
renderSquare (width, height) ((top, left), color) =
  li_
    [ class_ "grid-square-block"
    , style_ . M.fromList $
      [ ("top", top)
      , ("left", left)
      , ("width", width)
      , ("height", height)
      , ("position", "absolute")
      ]
    ]
    [ div_
        [ class_ "square-block"
        , style_ . M.fromList $
          [("background-color", color), ("width", "100%"), ("height", "100%")]
        ]
        []
    ]

renderTetromino :: [[Int]] -> MisoString -> View Action
renderTetromino shape color =
  ul_
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
    (map (renderSquare ("25%", "25%") . (, color) . conv') $ shapeToCoord shape)
  where
    conv = S.toMisoString . (++ "%") . show . (* 25)
    conv' = conv *** conv

renderNext :: Model -> View Action
renderNext model =
  div_
    [ class_ "next-tetromino"
    , style_ . M.fromList $
      [ ("padding", "0px")
      , ("margin-top", "8px")
      , ("overflow", "hidden")
      , ("position", "relative")
      , ("width", "92px")
      , ("height", "92px")
      ]
    ]
    [flip renderTetromino "#ecf0f1" . tetroShape $ TShaped]

renderActive :: Model -> View Action
renderActive model@Model {..} =
  div_
    [ class_ "active-tetromino"
    , style_ . M.fromList $
      [ ("top", conv (10 * y))
      , ("left", conv (10 * x))
      , ("width", "40%")
      , ("height", "20%")
      , ("position", "relative")
      ]
    ]
    [renderTetromino active color]
  where
    conv = S.toMisoString . (++ "%") . show

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
    [ div_
        [ class_ "well"
        , style_ . M.fromList $
          [ ("position", "relative")
          , ("width", "100%")
          , ("height", "100%")
          , ("overflow", "hidden")
          , ("display", "block")
          ]
        ]
        [renderActive model]
    ]

renderControlButton :: MisoString -> Action -> View Action
renderControlButton txt act =
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
    , onMouseDown act
    ]
    [text txt]

renderControls :: View Action
renderControls =
  div_
    [ class_ "controls"
    , style_ . M.fromList $
      [ ("height", "8%")
      , ("left", "0")
      , ("position", "absolute")
      , ("top", "600px")
      ]
    ]
    [ renderControlButton "↻" Rotate
    , renderControlButton "←" MoveLeft
    , renderControlButton "→" MoveRight
    , renderControlButton "↓" Accelerate
    ]

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

renderGameButton :: State -> View Action
renderGameButton state =
  let (txt, action) =
        case state of
          Paused -> ("Resume", Resume)
          Playing -> ("Pause", Pause)
          Stopped -> ("New Game", Start)
  in button_
       [ onClick action
       , style_ . M.fromList $
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
       [text txt]

renderPanel :: Model -> View Action
renderPanel model@Model {..} =
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
    , renderGameButton state
    ]

renderInfo :: State -> View Action
renderInfo state =
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
      , ("display", display)
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
  where
    display =
      case state of
        Playing -> "none"
        _ -> "block"

renderView :: Model -> View Action
renderView model@Model {..} =
  div_
    [class_ "flatris-game"]
    [renderWell model, renderControls, renderPanel model, renderInfo state]

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
    , div_
        [ class_ "preview"
        , style_ . M.fromList $
          [ ("left", "5px")
          , ("top", "30px")
          , ("overflow", "scroll")
          , ("width", "30%")
          , ("height", "680px")
          , ("position", "absolute")
          , ("background-color", "#34495f")
          , ("color", "#fff")
          ]
        ]
        [pre_ [] [text . S.toMisoString . encodePretty $ model]]
    ]
