module Controls exposing
    ( Handlers
    , ImportField(..)
    , Speed(..)
    , Status(..)
    , UserInput
    , view
    )

import GameOfLife exposing (GameOfLife, Zoom(..))
import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (autofocus, class, cols, placeholder, rows, value)
import Html.Events exposing (onClick, onInput)


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


type alias Handlers msg =
    { speedChange : msg
    , zoomChange : msg
    , statusChange : msg
    , undo : msg
    , redo : msg
    , randomize : msg
    , importFieldOpen : msg
    , importFieldChange : UserInput -> msg
    }


view :
    Status
    -> Speed
    -> Zoom
    -> GameOfLife
    -> ImportField
    -> Handlers msg
    -> Html msg
view status speed zoom gameOfLife importField handlers =
    div []
        [ div [ class "bottom-left-overlay" ]
            [ viewStatusButton status gameOfLife handlers.statusChange
            , viewSpeedButton speed handlers.speedChange
            , viewZoomButton zoom handlers.zoomChange
            , viewImportField importField handlers.importFieldOpen handlers.importFieldChange
            ]
        , div [ class "bottom-right-overlay" ]
            [ viewUndoButton status handlers.undo
            , viewRedoButton status handlers.redo
            , viewRandomizeButton handlers.randomize
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
