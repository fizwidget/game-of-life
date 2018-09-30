module Button exposing (view)

import Css exposing (..)
import Css.Transitions as Transitions exposing (easeInOut, transition)
import Html.Styled as Html exposing (Html, button, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


view : String -> msg -> List Style -> Html msg
view description clickMsg styles =
    button
        [ css (buttonStyles ++ styles)
        , onClick clickMsg
        ]
        [ text description ]


buttonStyles : List Style
buttonStyles =
    [ width (px 100)
    , height (px 40)
    , margin (px 5)
    , border2 (px 0) none
    , borderRadius (px 15)
    , color (rgb 255 255 255)
    , backgroundColor (rgba 179 186 197 0.6)
    , fontSize (px 20)
    , transition
        [ Transitions.backgroundColor3 200 0 easeInOut
        , Transitions.visibility3 200 0 easeInOut
        ]
    ]
