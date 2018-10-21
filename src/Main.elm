module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import Controls exposing (ImportField(..), Speed(..), Status(..), UserInput)
import GameOfLife exposing (GameOfLife, Padding(..), Size(..), Theme(..), Zoom(..))
import History exposing (History)
import Html exposing (Html, div, node, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode exposing (Decoder)
import Pattern exposing (Pattern)
import Random
import Time



-- MODEL


type Mouse
    = Up
    | Down


type alias Model =
    { game : History GameOfLife
    , status : Status
    , mouse : Mouse
    , speed : Speed
    , zoom : Zoom
    , theme : Theme
    , importField : ImportField
    }



-- INIT


defaultGameSize : Size
defaultGameSize =
    Size 20


init : ( Model, Cmd Msg )
init =
    withoutCmd
        { game = GameOfLife.begin defaultGameSize |> History.begin
        , status = Paused
        , mouse = Up
        , speed = Slow
        , zoom = Far
        , theme = Dark
        , importField = Closed
        }



-- UPDATE


type alias Coordinate =
    { x : Int
    , y : Int
    }


type Msg
    = ClockTick
    | StepBack
    | StepForward
    | MouseDown Coordinate
    | MouseOver Coordinate
    | MouseUp
    | ImportFieldOpen
    | ImportFieldChange UserInput
    | ImportFieldCancel
    | RandomPatternRequest
    | RandomPatternResponse Pattern
    | ChangeStatus
    | ChangeSpeed
    | ChangeZoom
    | ChangeTheme
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClockTick ->
            stepGame model
                |> ifGameFinished pauseGame
                |> withoutCmd

        StepBack ->
            tryUndoStep model
                |> Maybe.withDefault model
                |> pauseGame
                |> withoutCmd

        StepForward ->
            tryRedoStep model
                |> Maybe.withDefault (stepGame model)
                |> pauseGame
                |> withoutCmd

        MouseDown coordinate ->
            { model | mouse = Down }
                |> toggleCell coordinate
                |> withoutCmd

        MouseOver coordinate ->
            case model.mouse of
                Down ->
                    toggleCell coordinate model
                        |> withoutCmd

                Up ->
                    withoutCmd model

        MouseUp ->
            { model | mouse = Up }
                |> withoutCmd

        ImportFieldOpen ->
            { model | importField = Open "" }
                |> withoutCmd

        ImportFieldChange userInput ->
            case Pattern.parseLife106Format userInput of
                Ok parsedPattern ->
                    { model | importField = Closed, zoom = Far }
                        |> displayPattern WithPadding parsedPattern
                        |> withoutCmd

                Err _ ->
                    { model | importField = Open userInput }
                        |> withoutCmd

        ImportFieldCancel ->
            { model | importField = Closed }
                |> withoutCmd

        RandomPatternRequest ->
            ( model, requestRandomPattern defaultGameSize )

        RandomPatternResponse randomPattern ->
            displayPattern WithoutPadding randomPattern model
                |> withoutCmd

        ChangeStatus ->
            { model | status = nextStatus model.status }
                |> withoutCmd

        ChangeSpeed ->
            { model | speed = nextSpeed model.speed }
                |> withoutCmd

        ChangeZoom ->
            { model | zoom = nextZoom model.zoom }
                |> withoutCmd

        ChangeTheme ->
            { model | theme = nextTheme model.theme }
                |> withoutCmd

        NoOp ->
            withoutCmd model


withoutCmd : Model -> ( Model, Cmd msg )
withoutCmd model =
    ( model, Cmd.none )


pauseGame : Model -> Model
pauseGame model =
    { model | status = Paused }


stepGame : Model -> Model
stepGame model =
    History.record GameOfLife.step model.game
        |> setGame model


toggleCell : Coordinate -> Model -> Model
toggleCell coordinate model =
    History.record (GameOfLife.toggleCell coordinate) model.game
        |> setGame model


tryUndoStep : Model -> Maybe Model
tryUndoStep model =
    History.undo model.game
        |> Maybe.map (setGame model)


tryRedoStep : Model -> Maybe Model
tryRedoStep model =
    History.redo model.game
        |> Maybe.map (setGame model)


displayPattern : Padding -> Pattern -> Model -> Model
displayPattern padding pattern model =
    let
        gameWithPattern =
            GameOfLife.beginWithPattern defaultGameSize padding pattern
    in
    History.record (always gameWithPattern) model.game
        |> setGame model


requestRandomPattern : Size -> Cmd Msg
requestRandomPattern (Size size) =
    Pattern.generator { width = size, height = size }
        |> Random.generate RandomPatternResponse


ifGameFinished : (Model -> Model) -> Model -> Model
ifGameFinished updateModel model =
    if History.isUnchanged model.game then
        updateModel model

    else
        model


setGame : Model -> History GameOfLife -> Model
setGame model game =
    { model | game = game }


nextSpeed : Speed -> Speed
nextSpeed speed =
    case speed of
        Slow ->
            Medium

        Medium ->
            Fast

        Fast ->
            Slow


nextZoom : Zoom -> Zoom
nextZoom zoom =
    case zoom of
        Far ->
            Normal

        Normal ->
            Close

        Close ->
            Far


nextTheme : Theme -> Theme
nextTheme theme =
    case theme of
        Light ->
            Dark

        Dark ->
            Light


nextStatus : Status -> Status
nextStatus status =
    case status of
        Playing ->
            Paused

        Paused ->
            Playing



-- VIEW


document : Model -> Document Msg
document model =
    { title = "Game of Life"
    , body =
        [ bodyStyles model.theme
        , viewGame model
        , viewControls model
        ]
    }


bodyStyles : Theme -> Html msg
bodyStyles theme =
    let
        backgroundColor =
            case theme of
                Light ->
                    "white"

                Dark ->
                    "rgb(15, 15, 15)"

        backgroundColorStyle =
            "body { background-color: " ++ backgroundColor ++ "; }"
    in
    node "style" [] [ text backgroundColorStyle ]


viewGame : Model -> Html Msg
viewGame model =
    GameOfLife.view
        (History.now model.game)
        model.zoom
        model.theme
        gameEventHandlers


viewControls : Model -> Html Msg
viewControls model =
    Controls.view
        model.status
        model.importField
        controlEventHandlers


gameEventHandlers : GameOfLife.Events Msg
gameEventHandlers =
    { onMouseOver = MouseOver
    , onMouseDown = MouseDown
    , onMouseUp = MouseUp
    }


controlEventHandlers : Controls.Events Msg
controlEventHandlers =
    { onStepBack = StepBack
    , onStepForward = StepForward
    , onSpeedChange = ChangeSpeed
    , onZoomChange = ChangeZoom
    , onThemeChange = ChangeTheme
    , onStatusChange = ChangeStatus
    , onRandomize = RandomPatternRequest
    , onImportFieldOpen = ImportFieldOpen
    , onImportFieldChange = ImportFieldChange
    , onImportFieldCancel = ImportFieldCancel
    , noOp = NoOp
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ tickSubscription model.status model.speed
        , keyDownSubscription
        ]


tickSubscription : Status -> Speed -> Sub Msg
tickSubscription status speed =
    case status of
        Playing ->
            Time.every (tickInterval speed) (always ClockTick)

        Paused ->
            Sub.none


type alias Milliseconds =
    Float


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
    let
        keyDecoder =
            Decode.field "key" Decode.string

        onKeyDown =
            Controls.onKeyDown controlEventHandlers
    in
    Events.onKeyDown keyDecoder
        |> Sub.map onKeyDown



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = document
        , update = update
        , subscriptions = subscriptions
        }
