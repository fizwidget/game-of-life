module Overlay exposing (view)

import Css exposing (..)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)


type alias Sections msg =
    { bottomLeft : List (Html msg)
    , bottomRight : List (Html msg)
    }


view : Sections msg -> Html msg
view { bottomLeft, bottomRight } =
    div []
        [ viewBottomLeft bottomLeft
        , viewBottomRight bottomRight
        ]


viewBottomLeft : List (Html msg) -> Html msg
viewBottomLeft =
    div
        [ css
            [ position fixed
            , left sectionPadding
            , bottom sectionPadding
            , displayFlex
            , flexDirection column
            ]
        ]


viewBottomRight : List (Html msg) -> Html msg
viewBottomRight =
    div
        [ css
            [ position fixed
            , right sectionPadding
            , bottom sectionPadding
            ]
        ]


sectionPadding : Px
sectionPadding =
    px 20
