module Controls exposing
    ( Events
    , ImportField(..)
    , Speed(..)
    , Status(..)
    , UserInput
    , onKeyDown
    , view
    )

import GameOfLife exposing (GameOfLife, Zoom(..))
import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (autofocus, class, cols, placeholder, rows, value)
import Html.Events exposing (onClick, onInput)



-- TYPES


type ImportField
    = Open UserInput
    | Closed


type alias UserInput =
    String


type Status
    = Paused
    | Playing


type Speed
    = Slow
    | Medium
    | Fast


type alias Events msg =
    { onSpeedChange : msg
    , onZoomChange : msg
    , onStatusChange : msg
    , onUndo : msg
    , onRedo : msg
    , onRandomize : msg
    , onImportFieldOpen : msg
    , onImportFieldChange : UserInput -> msg
    , noOp : msg
    }



-- VIEW


view :
    GameOfLife
    -> Status
    -> Speed
    -> Zoom
    -> ImportField
    -> Events msg
    -> Html msg
view gameOfLife status speed zoom importField events =
    div []
        [ div [ class "bottom-left-overlay" ]
            [ viewStatusButton status gameOfLife events.onStatusChange
            , viewSpeedButton speed events.onSpeedChange
            , viewZoomButton zoom events.onZoomChange
            , viewImportField importField events.onImportFieldOpen events.onImportFieldChange
            ]
        , div [ class "bottom-right-overlay" ]
            [ viewUndoButton status events.onUndo
            , viewRedoButton status events.onRedo
            , viewRandomizeButton events.onRandomize
            ]
        ]


viewStatusButton : Status -> GameOfLife -> msg -> Html msg
viewStatusButton status gameOfLife clickMsg =
    case ( status, GameOfLife.isFinished gameOfLife ) of
        ( Paused, True ) ->
            viewButton "Play" clickMsg []

        ( Paused, False ) ->
            viewButton "Play" clickMsg [ class "green-button" ]

        ( Playing, _ ) ->
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


onKeyDown : Status -> Speed -> Zoom -> Events msg -> Key -> msg
onKeyDown status speed zoom events key =
    case key of
        "ArrowLeft" ->
            events.onUndo

        "ArrowRight" ->
            events.onRedo

        "p" ->
            events.onStatusChange

        "s" ->
            events.onSpeedChange

        "r" ->
            events.onRandomize

        "z" ->
            events.onZoomChange

        _ ->
            events.noOp
