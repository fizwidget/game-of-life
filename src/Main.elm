module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
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


type Status
    = Paused
    | Playing


type Speed
    = Slow
    | Medium
    | Fast


type Mouse
    = Up
    | Down


type ImportField
    = Open UserInput
    | Closed


type alias UserInput =
    String


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
                        |> setPattern parsedPattern
                        |> withoutCmd

        RandomPatternRequest ->
            model
                |> withCmd requestRandomPattern

        RandomPatternResponse randomPattern ->
            model
                |> setPattern randomPattern
                |> withoutCmd

        NoOp ->
            withoutCmd model


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


setPattern : Pattern -> Model -> Model
setPattern pattern model =
    let
        newSimulation =
            Simulation.startWithPattern pattern
    in
    { model | simulation = History.record (always newSimulation) model.simulation }


toggleCell : Coordinate -> Model -> Model
toggleCell coordinate model =
    History.record (Simulation.toggleCell coordinate) model.simulation
        |> setSimulationHistory model


stepSimulation : Model -> Model
stepSimulation model =
    History.record Simulation.step model.simulation
        |> setSimulationHistory model


withoutCmd : Model -> ( Model, Cmd msg )
withoutCmd model =
    ( model, Cmd.none )


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
    ( model, cmd )


tryUndo : Model -> Maybe Model
tryUndo model =
    History.undo model.simulation
        |> Maybe.map (setSimulationHistory model)


tryRedo : Model -> Maybe Model
tryRedo model =
    History.redo model.simulation
        |> Maybe.map (setSimulationHistory model)


setSimulationHistory : Model -> History Simulation -> Model
setSimulationHistory model simulation =
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
view { simulation, status, speed, zoom, importField } =
    let
        currentSimulation =
            History.now simulation

        handlers =
            { mouseOver = MouseOver
            , mouseDown = MouseDown
            , mouseUp = MouseUp
            }
    in
    div
        [ class "center-content" ]
        [ Simulation.view currentSimulation zoom handlers
        , viewControls status speed zoom currentSimulation importField
        ]


viewControls : Status -> Speed -> Zoom -> Simulation -> ImportField -> Html Msg
viewControls status speed zoom simulation importField =
    div []
        [ div [ class "bottom-left-overlay" ]
            [ viewStatusButton status simulation
            , viewSpeedButton speed
            , viewZoomButton zoom
            , viewImportField importField
            ]
        , div [ class "bottom-right-overlay" ]
            [ viewUndoButton status
            , viewRedoButton status
            , viewRandomizeButton
            ]
        ]


viewStatusButton : Status -> Simulation -> Html Msg
viewStatusButton status simulation =
    case ( status, Simulation.isFinished simulation ) of
        ( Paused, True ) ->
            viewButton "Play" (ChangeStatus Playing) []

        ( Paused, False ) ->
            viewButton "Play" (ChangeStatus Playing) [ class "green-button" ]

        ( Playing, _ ) ->
            viewButton "Pause" (ChangeStatus Paused) []


viewSpeedButton : Speed -> Html Msg
viewSpeedButton speed =
    let
        onClick =
            ChangeSpeed (nextSpeed speed)
    in
    case speed of
        Slow ->
            viewButton "Slow" onClick []

        Medium ->
            viewButton "Medium" onClick []

        Fast ->
            viewButton "Fast" onClick []


viewZoomButton : Zoom -> Html Msg
viewZoomButton zoom =
    let
        onClick =
            ChangeZoom (nextZoomLevel zoom)
    in
    case zoom of
        Far ->
            viewButton "1X" onClick []

        Normal ->
            viewButton "1.5X" onClick []

        Close ->
            viewButton "2X" onClick []


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
                , class "import-field"
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


viewRandomizeButton : Html Msg
viewRandomizeButton =
    viewButton "Random" RandomPatternRequest []


viewButton : String -> msg -> List (Attribute msg) -> Html msg
viewButton description clickMsg customAttributes =
    let
        attributes =
            [ class "button", onClick clickMsg ] ++ customAttributes
    in
    button attributes [ text description ]


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
