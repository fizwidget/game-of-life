module Controls exposing
    ( ImportField(..)
    , Status(..)
    , UserInput
    , onKeyDown
    , view
    )

import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (autofocus, class, classList, placeholder, title, value)
import Html.Events exposing (onClick, onInput)
import Msg exposing (Msg(..))



-- TYPES


type Status
    = Paused
    | Playing


type ImportField
    = Closed
    | Open UserInput


type alias UserInput =
    String



-- VIEW


view : Status -> ImportField -> Html Msg
view status importField =
    div []
        [ div [ class "control-panel" ]
            [ viewStatusButton status
            , viewBackButton
            , viewForwardButton
            , viewSpeedButton
            , viewZoomButton
            , viewRandomizeButton
            , viewThemeButton
            , viewImportButton importField
            ]
        , viewImportField importField
        ]


viewStatusButton : Status -> Html Msg
viewStatusButton status =
    case status of
        Paused ->
            viewButton
                { label = "Start"
                , tooltip = "Start simulation (P)"
                , onClick = ChangeStatus
                , attributes = [ class "play-button" ]
                }

        Playing ->
            viewButton
                { label = "Stop"
                , tooltip = "Stop simulation (P)"
                , onClick = ChangeStatus
                , attributes = []
                }


viewBackButton : Html Msg
viewBackButton =
    viewButton
        { label = "⇦"
        , tooltip = "Back (←)"
        , onClick = StepBack
        , attributes = []
        }


viewForwardButton : Html Msg
viewForwardButton =
    viewButton
        { label = "⇨"
        , tooltip = "Forward (→)"
        , onClick = StepForward
        , attributes = []
        }


viewSpeedButton : Html Msg
viewSpeedButton =
    viewButton
        { label = "🏃\u{200D}♀️"
        , tooltip = "Speed (S)"
        , onClick = ChangeSpeed
        , attributes = []
        }


viewZoomButton : Html Msg
viewZoomButton =
    viewButton
        { label = "🔬"
        , tooltip = "Zoom (Z)"
        , onClick = ChangeZoom
        , attributes = []
        }


viewRandomizeButton : Html Msg
viewRandomizeButton =
    viewButton
        { label = "🎲"
        , tooltip = "Randomize (R)"
        , onClick = RandomPatternRequest
        , attributes = []
        }


viewThemeButton : Html Msg
viewThemeButton =
    viewButton
        { label = "🎨"
        , tooltip = "Theme (T)"
        , onClick = ChangeTheme
        , attributes = []
        }


viewImportButton : ImportField -> Html Msg
viewImportButton importField =
    case importField of
        Closed ->
            viewButton
                { label = "Import"
                , tooltip = "Import pattern"
                , onClick = ImportFieldOpen
                , attributes = []
                }

        Open _ ->
            viewButton
                { label = "Cancel"
                , tooltip = "Cancel import"
                , onClick = ImportFieldCancel
                , attributes = []
                }


viewImportField : ImportField -> Html Msg
viewImportField importField =
    case importField of
        Closed ->
            text ""

        Open userInput ->
            textarea
                [ autofocus True
                , placeholder "Paste a 'Life 1.06' pattern here..."
                , classList
                    [ ( "import-field", True )
                    , ( "invalid", not <| String.isEmpty userInput )
                    ]
                , value userInput
                , onInput ImportFieldChange
                ]
                []


type alias ButtonConfig =
    { label : String
    , tooltip : String
    , onClick : Msg
    , attributes : List (Attribute Msg)
    }


viewButton : ButtonConfig -> Html Msg
viewButton { label, tooltip, onClick, attributes } =
    let
        baseAttributes =
            [ class "button", title tooltip, Html.Events.onClick onClick ]
    in
    button (baseAttributes ++ attributes) [ text label ]



-- KEYBOARD


type alias Key =
    String


onKeyDown : Key -> Msg
onKeyDown key =
    case key of
        "ArrowLeft" ->
            StepBack

        "ArrowRight" ->
            StepForward

        "p" ->
            ChangeStatus

        "s" ->
            ChangeSpeed

        "r" ->
            RandomPatternRequest

        "z" ->
            ChangeZoom

        "t" ->
            ChangeTheme

        _ ->
            NoOp
