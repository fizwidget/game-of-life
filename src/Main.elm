module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import History exposing (History)
import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (autofocus, class, cols, placeholder, rows, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Pattern exposing (Pattern)
import Simulation exposing (Simulation)
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
    | Medium
    | Fast


type ImportField
    = Open String
    | Closed


type alias Model =
    { simulation : History Simulation
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
    , simulation = History.begin Simulation.begin
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
            { model | simulation = History.record Simulation.step model.simulation }
                |> pauseIfUnchanged

        Undo ->
            undo model

        Redo ->
            redoOrStep model

        SetSpeed speed ->
            { model | speed = speed }

        MouseDown coordinate ->
            { model
                | mouse = Down
                , simulation = toggleCell coordinate model.simulation
            }

        MouseUp ->
            { model | mouse = Up }

        MouseOver coordinate ->
            case model.mouse of
                Down ->
                    { model | simulation = toggleCell coordinate model.simulation }

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
            case parsePattern text of
                Nothing ->
                    { model | importField = Open text }

                Just newSimulation ->
                    { model
                        | importField = Closed
                        , simulation = History.record (always newSimulation) model.simulation
                    }


undo : Model -> Model
undo model =
    { model
        | status = Paused
        , simulation =
            History.undo model.simulation
                |> Maybe.withDefault model.simulation
    }


redoOrStep : Model -> Model
redoOrStep model =
    { model
        | status = Paused
        , simulation =
            History.redo model.simulation
                |> Maybe.withDefault (History.record Simulation.step model.simulation)
    }


toggleStatus : Model -> Model
toggleStatus model =
    case model.status of
        Playing ->
            { model | status = Paused }

        Paused ->
            { model | status = Playing }


pauseIfUnchanged : Model -> Model
pauseIfUnchanged model =
    if History.isUnchanged model.simulation then
        { model | status = Paused }

    else
        model


toggleCell : Coordinate -> History Simulation -> History Simulation
toggleCell coordinate simulation =
    History.record (Simulation.toggleCell coordinate) simulation


parsePattern : String -> Maybe Simulation
parsePattern text =
    Pattern.parseLife106 text
        |> Maybe.map Simulation.beginWithPattern



-- VIEW


document : Model -> Document Msg
document model =
    { title = "Game of Life"
    , body = [ view model ]
    }


view : Model -> Html Msg
view { simulation, status, speed, importField } =
    let
        currentSimulation =
            History.now simulation

        transitionDuration =
            calculateTransitionDuration speed

        handlers =
            { mouseOver = MouseOver
            , mouseDown = MouseDown
            , mouseUp = MouseUp
            }
    in
    div
        [ class "center" ]
        [ Simulation.view transitionDuration currentSimulation handlers
        , viewControls status speed currentSimulation importField
        ]


calculateTransitionDuration : Speed -> Milliseconds
calculateTransitionDuration speed =
    tickInterval speed + 200


viewControls : Status -> Speed -> Simulation -> ImportField -> Html Msg
viewControls status speed simulation importField =
    div []
        [ div [ class "bottom-left" ]
            [ viewImportField importField
            , viewStatusButton status |> hideIfFinished simulation
            , viewSpeedButton speed
            ]
        , div [ class "bottom-right" ]
            [ viewUndoButton status
            , viewRedoButton status
            ]
        ]


hideIfFinished : Simulation -> Html msg -> Html msg
hideIfFinished simulation children =
    if Simulation.isFinished simulation then
        div [] []

    else
        children


viewStatusButton : Status -> Html Msg
viewStatusButton status =
    case status of
        Playing ->
            viewButton "Pause" Pause []

        Paused ->
            viewButton "Play" Play [ class "play-button" ]


viewSpeedButton : Speed -> Html Msg
viewSpeedButton speed =
    case speed of
        Slow ->
            viewButton "Medium" (SetSpeed Medium) []

        Medium ->
            viewButton "Fast" (SetSpeed Fast) []

        Fast ->
            viewButton "Slow" (SetSpeed Slow) []


viewImportField : ImportField -> Html Msg
viewImportField importField =
    case importField of
        Closed ->
            viewButton "Import" ImportFieldOpen []

        Open text ->
            textarea
                [ rows 22
                , cols 30
                , autofocus True
                , placeholder "Paste a 'Life 1.06' pattern here"
                , class "importer"
                , value text
                , onInput ImportFieldChange
                ]
                []


viewUndoButton : Status -> Html Msg
viewUndoButton status =
    viewButton "⬅︎" Undo []


viewRedoButton : Status -> Html Msg
viewRedoButton status =
    viewButton "➡︎" Redo []


viewButton : String -> msg -> List (Attribute msg) -> Html msg
viewButton description clickMsg attrs =
    button
        ([ class "button", onClick clickMsg ] ++ attrs)
        [ text description ]



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

        Medium ->
            300

        Fast ->
            50


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
