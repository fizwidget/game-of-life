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
import Html.Attributes exposing (autofocus, class, placeholder, value)
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
    , onImportFieldCancel : msg
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
    div []
        [ div [ class "control-panel" ]
            [ viewStatusButton status events.onStatusChange
            , viewBackButton status events.onStepBack
            , viewForwardButton status events.onStepForward
            , viewZoomButton zoom events.onZoomChange
            , viewSpeedButton speed events.onSpeedChange
            , viewRandomizeButton events.onRandomize
            , viewThemeButton theme events.onThemeChange
            , viewImportButton importField
                events.onImportFieldOpen
                events.onImportFieldCancel
            ]
        , viewImportField importField events.onImportFieldChange
        ]


viewStatusButton : Status -> msg -> Html msg
viewStatusButton status clickMsg =
    case status of
        Paused ->
            viewButton "Play" clickMsg [ class "play-button" ]

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


viewImportButton : ImportField -> msg -> msg -> Html msg
viewImportButton importField openMsg cancelMsg =
    case importField of
        Open text ->
            viewButton "Cancel" cancelMsg []

        Closed ->
            viewButton "Import" openMsg []


viewImportField : ImportField -> (UserInput -> msg) -> Html msg
viewImportField importField changeMsg =
    case importField of
        Open text ->
            textarea
                [ autofocus True
                , placeholder "Paste a 'Life 1.06' pattern here..."
                , class "import-field"
                , value text
                , onInput changeMsg
                ]
                []

        Closed ->
            text ""


viewBackButton : Status -> msg -> Html msg
viewBackButton status clickMsg =
    viewButton "⬅︎" clickMsg []


viewForwardButton : Status -> msg -> Html msg
viewForwardButton status clickMsg =
    viewButton "➡︎" clickMsg []


viewRandomizeButton : msg -> Html msg
viewRandomizeButton clickMsg =
    viewButton "Randomize" clickMsg []


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
