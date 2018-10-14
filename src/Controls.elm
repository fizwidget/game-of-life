module Controls exposing
    ( Events
    , ImportField(..)
    , Speed(..)
    , Status(..)
    , UserInput
    , onKeyDown
    , view
    )

import Common exposing (Theme(..), Zoom(..))
import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (autofocus, class, cols, placeholder, rows, value)
import Html.Events exposing (onClick, onInput)



-- TYPES


type Status
    = Paused
    | Playing


type Speed
    = Slow
    | Medium
    | Fast


type ImportField
    = Open UserInput
    | Closed


type alias UserInput =
    String


type alias Events msg =
    { onStepBack : msg
    , onStepForward : msg
    , onStatusChange : msg
    , onSpeedChange : msg
    , onZoomChange : msg
    , onThemeChange : msg
    , onRandomize : msg
    , onImportFieldOpen : msg
    , onImportFieldChange : UserInput -> msg
    , noOp : msg
    }



-- VIEW


view :
    Status
    -> Speed
    -> Zoom
    -> Theme
    -> ImportField
    -> Events msg
    -> Html msg
view status speed zoom theme importField events =
    div [ class "control-panel" ]
        [ viewStatusButton status events.onStatusChange
        , viewUndoButton status events.onStepBack
        , viewZoomButton zoom events.onZoomChange
        , viewThemeButton theme events.onThemeChange
        , viewSpeedButton speed events.onSpeedChange
        , viewRedoButton status events.onStepForward
        , viewRandomizeButton events.onRandomize
        , viewImportField importField events.onImportFieldOpen events.onImportFieldChange
        ]


viewStatusButton : Status -> msg -> Html msg
viewStatusButton status clickMsg =
    case status of
        Paused ->
            viewButton "Play" clickMsg [ class "green-button" ]

        Playing ->
            viewButton "Pause" clickMsg []


viewSpeedButton : Speed -> msg -> Html msg
viewSpeedButton speed clickMsg =
    case speed of
        Slow ->
            viewButton "Slow" clickMsg []

        Medium ->
            viewButton "Medium" clickMsg []

        Fast ->
            viewButton "Fast" clickMsg []


viewZoomButton : Zoom -> msg -> Html msg
viewZoomButton zoom clickMsg =
    case zoom of
        Far ->
            viewButton "1X" clickMsg []

        Normal ->
            viewButton "1.5X" clickMsg []

        Close ->
            viewButton "2X" clickMsg []


viewThemeButton : Theme -> msg -> Html msg
viewThemeButton theme clickMsg =
    case theme of
        Light ->
            viewButton "Light" clickMsg []

        Dark ->
            viewButton "Dark" clickMsg []


viewImportField : ImportField -> msg -> (UserInput -> msg) -> Html msg
viewImportField importField openMsg changeMsg =
    case importField of
        Closed ->
            viewButton "Import" openMsg []

        Open text ->
            textarea
                [ rows 22
                , cols 30
                , autofocus True
                , placeholder "Paste a 'Life 1.06' pattern here"
                , class "import-field"
                , value text
                , onInput changeMsg
                ]
                []


viewUndoButton : Status -> msg -> Html msg
viewUndoButton status clickMsg =
    viewButton "⬅︎" clickMsg []


viewRedoButton : Status -> msg -> Html msg
viewRedoButton status clickMsg =
    viewButton "➡︎" clickMsg []


viewRandomizeButton : msg -> Html msg
viewRandomizeButton clickMsg =
    viewButton "Random" clickMsg []


viewButton : String -> msg -> List (Attribute msg) -> Html msg
viewButton description clickMsg customAttributes =
    let
        attributes =
            [ class "button", onClick clickMsg ] ++ customAttributes
    in
    button attributes [ text description ]



-- KEYBOARD


type alias Key =
    String


onKeyDown : Events msg -> Key -> msg
onKeyDown events key =
    case key of
        "ArrowLeft" ->
            events.onStepBack

        "ArrowRight" ->
            events.onStepForward

        "p" ->
            events.onStatusChange

        "s" ->
            events.onSpeedChange

        "r" ->
            events.onRandomize

        "z" ->
            events.onZoomChange

        "t" ->
            events.onThemeChange

        _ ->
            events.noOp
