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
import Html.Attributes exposing (autofocus, class, placeholder, title, value)
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


view : Status -> ImportField -> Events msg -> Html msg
view status importField events =
    div []
        [ div [ class "control-panel" ]
            [ viewStatusButton status events.onStatusChange
            , viewBackButton status events.onStepBack
            , viewForwardButton status events.onStepForward
            , viewRandomizeButton events.onRandomize
            , viewSpeedButton events.onSpeedChange
            , viewZoomButton events.onZoomChange
            , viewThemeButton events.onThemeChange
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
            viewButton "Start" "Start simulation" clickMsg [ class "play-button" ]

        Playing ->
            viewButton "Stop" "Stop simulation" clickMsg []


viewSpeedButton : msg -> Html msg
viewSpeedButton clickMsg =
    viewButton "🏃\u{200D}♀️" "Speed" clickMsg []


viewZoomButton : msg -> Html msg
viewZoomButton clickMsg =
    viewButton "🔬" "Zoom" clickMsg []


viewThemeButton : msg -> Html msg
viewThemeButton clickMsg =
    viewButton "🎨" "Theme" clickMsg []


viewImportButton : ImportField -> msg -> msg -> Html msg
viewImportButton importField openMsg cancelMsg =
    case importField of
        Open text ->
            viewButton "Cancel" "Cancel import" cancelMsg []

        Closed ->
            viewButton "Import" "Import pattern" openMsg []


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
    viewButton "⇦" "Back" clickMsg []


viewForwardButton : Status -> msg -> Html msg
viewForwardButton status clickMsg =
    viewButton "⇨" "Forward" clickMsg []


viewRandomizeButton : msg -> Html msg
viewRandomizeButton clickMsg =
    viewButton "🎲" "Randomize" clickMsg []


viewButton : String -> String -> msg -> List (Attribute msg) -> Html msg
viewButton description tooltip clickMsg customAttributes =
    let
        attributes =
            [ class "button", title tooltip, onClick clickMsg ] ++ customAttributes
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
