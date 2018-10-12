module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import Controls exposing (ImportField(..), Speed(..), Status(..), UserInput)
import History exposing (History)
import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (autofocus, class, cols, placeholder, rows, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Pattern exposing (Pattern)
import Random
import Simulation exposing (Simulation, Zoom(..))
import Time



-- MODEL


type Mouse
    = Up
    | Down


type alias Model =
    { simulation : History Simulation
    , status : Status
    , mouse : Mouse
    , speed : Speed
    , zoom : Zoom
    , importField : ImportField
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    withoutCmd initialModel


initialModel : Model
initialModel =
    { status = Paused
    , simulation = History.begin Simulation.start
    , mouse = Up
    , speed = Slow
    , zoom = Far
    , importField = Closed
    }



-- UPDATE


type Msg
    = Undo
    | Redo
    | ClockTick
    | ChangeStatus Status
    | ChangeSpeed Speed
    | ChangeZoom Zoom
    | MouseDown Coordinate
    | MouseOver Coordinate
    | MouseUp
    | ImportFieldOpen
    | ImportFieldChange UserInput
    | RandomPatternRequest
    | RandomPatternResponse Pattern
    | NoOp


type alias Coordinate =
    { x : Int
    , y : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Undo ->
            model
                |> pauseSimulation
                |> tryUndo
                |> Maybe.withDefault model
                |> withoutCmd

        Redo ->
            model
                |> pauseSimulation
                |> tryRedo
                |> Maybe.withDefault (stepSimulation model)
                |> withoutCmd

        ClockTick ->
            model
                |> stepSimulation
                |> pauseIfUnchanged
                |> withoutCmd

        ChangeStatus status ->
            { model | status = status }
                |> withoutCmd

        ChangeSpeed speed ->
            { model | speed = speed }
                |> withoutCmd

        ChangeZoom zoom ->
            { model | zoom = zoom }
                |> withoutCmd

        MouseDown coordinate ->
            { model | mouse = Down }
                |> toggleCell coordinate
                |> withoutCmd

        MouseOver coordinate ->
            case model.mouse of
                Down ->
                    model
                        |> toggleCell coordinate
                        |> withoutCmd

                Up ->
                    withoutCmd model

        MouseUp ->
            { model | mouse = Up }
                |> withoutCmd

        ImportFieldOpen ->
            model
                |> openImportField
                |> withoutCmd

        ImportFieldChange userInput ->
            case Pattern.parseLife106 userInput of
                Nothing ->
                    model
                        |> setImportField userInput
                        |> withoutCmd

                Just parsedPattern ->
                    model
                        |> closeImportField
                        |> resetZoom
                        |> displayPattern parsedPattern
                        |> withoutCmd

        RandomPatternRequest ->
            model
                |> withCmd requestRandomPattern

        RandomPatternResponse randomPattern ->
            model
                |> displayPattern randomPattern
                |> withoutCmd

        NoOp ->
            withoutCmd model



-- UPDATE HELPERS


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
    ( model, cmd )


withoutCmd : Model -> ( Model, Cmd msg )
withoutCmd model =
    ( model, Cmd.none )


pauseSimulation : Model -> Model
pauseSimulation model =
    { model | status = Paused }


resetZoom : Model -> Model
resetZoom model =
    { model | zoom = Far }


openImportField : Model -> Model
openImportField model =
    { model | importField = Open "" }


setImportField : UserInput -> Model -> Model
setImportField userInput model =
    { model | importField = Open userInput }


closeImportField : Model -> Model
closeImportField model =
    { model | importField = Closed }


displayPattern : Pattern -> Model -> Model
displayPattern pattern model =
    let
        newSimulation =
            Simulation.startWithPattern pattern
    in
    History.record (always newSimulation) model.simulation
        |> setSimulation model


toggleCell : Coordinate -> Model -> Model
toggleCell coordinate model =
    History.record (Simulation.toggleCell coordinate) model.simulation
        |> setSimulation model


stepSimulation : Model -> Model
stepSimulation model =
    History.record Simulation.step model.simulation
        |> setSimulation model


tryUndo : Model -> Maybe Model
tryUndo model =
    History.undo model.simulation
        |> Maybe.map (setSimulation model)


tryRedo : Model -> Maybe Model
tryRedo model =
    History.redo model.simulation
        |> Maybe.map (setSimulation model)


setSimulation : Model -> History Simulation -> Model
setSimulation model simulation =
    { model | simulation = simulation }


pauseIfUnchanged : Model -> Model
pauseIfUnchanged model =
    if History.isUnchanged model.simulation then
        pauseSimulation model

    else
        model


requestRandomPattern : Cmd Msg
requestRandomPattern =
    Random.generate RandomPatternResponse Pattern.generator



-- VIEW


document : Model -> Document Msg
document model =
    { title = "Game of Life"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    let
        simulation =
            History.now model.simulation

        simulationView =
            Simulation.view
                simulation
                model.zoom
                simulationHandlers

        controlsView =
            Controls.view
                model.status
                model.speed
                model.zoom
                simulation
                model.importField
                (controlHandlers model)
    in
    div
        [ class "center-content" ]
        [ simulationView, controlsView ]


simulationHandlers : Simulation.Handlers Msg
simulationHandlers =
    { mouseOver = MouseOver
    , mouseDown = MouseDown
    , mouseUp = MouseUp
    }


controlHandlers : Model -> Controls.Handlers Msg
controlHandlers { speed, zoom, status } =
    { speedChange = ChangeSpeed (nextSpeed speed)
    , zoomChange = ChangeZoom (nextZoomLevel zoom)
    , statusChange = ChangeStatus (nextStatus status)
    , undo = Undo
    , redo = Redo
    , randomize = RandomPatternRequest
    , importFieldOpen = ImportFieldOpen
    , importFieldChange = ImportFieldChange
    }


nextSpeed : Speed -> Speed
nextSpeed speed =
    case speed of
        Slow ->
            Medium

        Medium ->
            Fast

        Fast ->
            Slow


nextZoomLevel : Zoom -> Zoom
nextZoomLevel zoom =
    case zoom of
        Far ->
            Normal

        Normal ->
            Close

        Close ->
            Far


nextStatus : Status -> Status
nextStatus status =
    case status of
        Playing ->
            Paused

        Paused ->
            Playing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { status, speed, zoom } =
    Sub.batch
        [ keyDownSubscription status speed zoom
        , tickSubscription status speed
        ]


tickSubscription : Status -> Speed -> Sub Msg
tickSubscription status speed =
    case status of
        Playing ->
            Time.every (tickInterval speed) (always ClockTick)

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


keyDownSubscription : Status -> Speed -> Zoom -> Sub Msg
keyDownSubscription status speed zoom =
    Events.onKeyDown keyDecoder
        |> Sub.map (toMsg status speed zoom)


keyDecoder : Decoder Key
keyDecoder =
    Decode.field "key" Decode.string


toMsg : Status -> Speed -> Zoom -> Key -> Msg
toMsg status speed zoom key =
    case key of
        "ArrowLeft" ->
            Undo

        "ArrowRight" ->
            Redo

        "p" ->
            ChangeStatus <|
                case status of
                    Playing ->
                        Paused

                    Paused ->
                        Playing

        "s" ->
            ChangeSpeed (nextSpeed speed)

        "r" ->
            RandomPatternRequest

        "z" ->
            ChangeZoom (nextZoomLevel zoom)

        _ ->
            NoOp


type alias Key =
    String


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
