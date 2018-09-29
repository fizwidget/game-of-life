module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import Button
import Css exposing (..)
import History exposing (History)
import Html.Styled as Html exposing (Html, button, div, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, cols, css, disabled, placeholder, rows, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra as Maybe
import Overlay
import Pattern exposing (Pattern)
import Simulation exposing (Cell(..), Cells)
import Time



-- MODEL


type Status
    = Paused
    | Playing


type Mouse
    = Up
    | Down


type Speed
    = Slow
    | Fast


type ImportField
    = Open String
    | Closed


type alias Model =
    { cells : History Cells
    , status : Status
    , mouse : Mouse
    , speed : Speed
    , importField : ImportField
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { status = Paused
    , cells = History.begin Simulation.begin
    , mouse = Up
    , speed = Slow
    , importField = Closed
    }



-- UPDATE


type Msg
    = Play
    | Pause
    | Step
    | Undo
    | Redo
    | SetSpeed Speed
    | MouseDown Coordinate
    | MouseUp
    | MouseOver Coordinate
    | KeyDown Key
    | ImportFieldOpen
    | ImportFieldChange String


type Key
    = LeftKey
    | RightKey
    | PKey
    | OtherKey


type alias Coordinate =
    { x : Int
    , y : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Play ->
            { model | status = Playing }

        Pause ->
            { model | status = Paused }

        Step ->
            { model | cells = History.record Simulation.step model.cells }
                |> pauseIfStable

        Undo ->
            undo model

        Redo ->
            redoOrStep model

        SetSpeed speed ->
            { model | speed = speed }

        MouseDown coordinate ->
            { model
                | mouse = Down
                , cells = toggleCell coordinate model.cells
            }

        MouseUp ->
            { model | mouse = Up }

        MouseOver coordinate ->
            case model.mouse of
                Down ->
                    { model | cells = toggleCell coordinate model.cells }

                Up ->
                    model

        KeyDown key ->
            case key of
                LeftKey ->
                    undo model

                RightKey ->
                    redoOrStep model

                PKey ->
                    toggleStatus model

                OtherKey ->
                    model

        ImportFieldOpen ->
            { model | importField = Open "" }

        ImportFieldChange text ->
            case parseCells text of
                Nothing ->
                    { model | importField = Open text }

                Just parsedCells ->
                    { model
                        | importField = Closed
                        , cells = History.record (always parsedCells) model.cells
                    }


undo : Model -> Model
undo model =
    { model
        | status = Paused
        , cells = History.undo model.cells
    }


redoOrStep : Model -> Model
redoOrStep model =
    { model
        | status = Paused
        , cells =
            History.redo model.cells
                |> Maybe.withDefault (History.record Simulation.step model.cells)
    }


toggleStatus : Model -> Model
toggleStatus model =
    case model.status of
        Playing ->
            { model | status = Paused }

        Paused ->
            { model | status = Playing }


pauseIfStable : Model -> Model
pauseIfStable model =
    if History.isStable model.cells then
        { model | status = Paused }

    else
        model


toggleCell : Coordinate -> History Cells -> History Cells
toggleCell coordinate cells =
    History.record (Simulation.toggleCell coordinate) cells


parseCells : String -> Maybe Cells
parseCells text =
    Pattern.parseLife106 text
        |> Maybe.map Simulation.beginWithPattern



-- VIEW


document : Model -> Document Msg
document model =
    { title = "Game of Life"
    , body = [ view model |> toUnstyled ]
    }


view : Model -> Html Msg
view { cells, status, speed, importField } =
    let
        currentCells =
            History.now cells

        transitionDuration =
            calculateTransitionDuration speed

        handlers =
            { mouseOver = MouseOver
            , mouseDown = MouseDown
            , mouseUp = MouseUp
            }
    in
    div
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]
        [ Simulation.view transitionDuration currentCells handlers
        , viewControls status speed currentCells importField
        ]


calculateTransitionDuration : Speed -> Milliseconds
calculateTransitionDuration speed =
    tickInterval speed + 200


viewControls : Status -> Speed -> Cells -> ImportField -> Html Msg
viewControls status speed cells importField =
    Overlay.view
        { bottomLeft =
            [ viewImportField importField
            , viewStatusButton status |> unlessSimulationFinished cells
            , viewSpeedButton speed
            ]
        , bottomRight =
            [ viewUndoButton status
            , viewRedoButton status
            ]
        }


unlessSimulationFinished : Cells -> Html msg -> Html msg
unlessSimulationFinished cells children =
    if Simulation.isFinished cells then
        div [] []

    else
        children


viewStatusButton : Status -> Html Msg
viewStatusButton status =
    case status of
        Playing ->
            Button.view "Pause" Pause []

        Paused ->
            Button.view "Play" Play [ backgroundColor (rgba 54 179 126 0.8) ]


viewSpeedButton : Speed -> Html Msg
viewSpeedButton speed =
    case speed of
        Slow ->
            Button.view "Faster" (SetSpeed Fast) []

        Fast ->
            Button.view "Slower" (SetSpeed Slow) []


viewImportField : ImportField -> Html Msg
viewImportField importField =
    case importField of
        Closed ->
            Button.view "Import" ImportFieldOpen []

        Open text ->
            textarea
                [ rows 22
                , cols 30
                , autofocus True
                , placeholder "Paste a 'Life 1.06' pattern here"
                , css [ borderRadius (px 4), resize none ]
                , value text
                , onInput ImportFieldChange
                ]
                []


viewUndoButton : Status -> Html Msg
viewUndoButton status =
    Button.view "⬅︎" Undo []


viewRedoButton : Status -> Html Msg
viewRedoButton status =
    Button.view "➡︎" Redo []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { status, speed } =
    Sub.batch
        [ keyDownSubscription
        , tickSubscription status speed
        ]


tickSubscription : Status -> Speed -> Sub Msg
tickSubscription status speed =
    case status of
        Playing ->
            Time.every (tickInterval speed) (always Step)

        Paused ->
            Sub.none


tickInterval : Speed -> Milliseconds
tickInterval speed =
    case speed of
        Slow ->
            600

        Fast ->
            300


keyDownSubscription : Sub Msg
keyDownSubscription =
    Events.onKeyDown keyDecoder
        |> Sub.map KeyDown


keyDecoder : Decoder Key
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map toKey


toKey : String -> Key
toKey value =
    case value of
        "ArrowLeft" ->
            LeftKey

        "ArrowRight" ->
            RightKey

        "p" ->
            PKey

        _ ->
            OtherKey


type alias Milliseconds =
    Float



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = document
        , update = update
        , subscriptions = subscriptions
        }
