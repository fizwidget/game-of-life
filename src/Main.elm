module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import Controls exposing (ImportField(..), Speed(..), Status(..), UserInput)
import GameOfLife exposing (GameOfLife, Zoom(..))
import History exposing (History)
import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (autofocus, class, cols, placeholder, rows, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Pattern exposing (Pattern)
import Random
import Time



-- MODEL


type Mouse
    = Up
    | Down


type alias Model =
    { gameOfLife : History GameOfLife
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
    , gameOfLife = History.begin GameOfLife.begin
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
                |> pause
                |> maybeUndo
                |> Maybe.withDefault model
                |> withoutCmd

        Redo ->
            model
                |> pause
                |> maybeRedo
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


pause : Model -> Model
pause model =
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
        newGame =
            GameOfLife.beginWithPattern pattern
    in
    History.record (always newGame) model.gameOfLife
        |> setGame model


toggleCell : Coordinate -> Model -> Model
toggleCell coordinate model =
    History.record (GameOfLife.toggleCell coordinate) model.gameOfLife
        |> setGame model


stepSimulation : Model -> Model
stepSimulation model =
    History.record GameOfLife.step model.gameOfLife
        |> setGame model


maybeUndo : Model -> Maybe Model
maybeUndo model =
    History.undo model.gameOfLife
        |> Maybe.map (setGame model)


maybeRedo : Model -> Maybe Model
maybeRedo model =
    History.redo model.gameOfLife
        |> Maybe.map (setGame model)


setGame : Model -> History GameOfLife -> Model
setGame model gameOfLife =
    { model | gameOfLife = gameOfLife }


pauseIfUnchanged : Model -> Model
pauseIfUnchanged model =
    if History.isUnchanged model.gameOfLife then
        pause model

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
    div
        [ class "center-content" ]
        [ viewGame model, viewControls model ]


viewGame : Model -> Html Msg
viewGame model =
    GameOfLife.view
        (History.now model.gameOfLife)
        model.zoom
        gameOfLifeHandlers


viewControls : Model -> Html Msg
viewControls model =
    Controls.view
        model.status
        model.speed
        model.zoom
        (History.now model.gameOfLife)
        model.importField
        (controlHandlers model)


gameOfLifeHandlers : GameOfLife.Handlers Msg
gameOfLifeHandlers =
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
