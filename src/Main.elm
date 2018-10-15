module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import Common exposing (Theme(..), Zoom(..))
import Controls exposing (ImportField(..), Speed(..), Status(..), UserInput)
import GameOfLife exposing (GameOfLife, Padding(..))
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


type alias Dimensions =
    { width : Int
    , height : Int
    }


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


init : ( Model, Cmd Msg )
init =
    withoutCmd initialModel


initialModel : Model
initialModel =
    { status = Paused
    , game = History.begin (GameOfLife.begin defaultGameSize)
    , mouse = Up
    , speed = Slow
    , zoom = Far
    , theme = Dark
    , importField = Closed
    }


defaultGameSize : Dimensions
defaultGameSize =
    { width = 18
    , height = 18
    }



-- UPDATE


type Msg
    = ClockTick
    | StepBack
    | StepForward
    | ChangeStatus Status
    | ChangeSpeed Speed
    | ChangeZoom Zoom
    | ChangeTheme Theme
    | MouseDown Coordinate
    | MouseOver Coordinate
    | MouseUp
    | ImportFieldOpen
    | ImportFieldChange UserInput
    | ImportFieldCancel
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

        ChangeStatus status ->
            { model | status = status }
                |> withoutCmd

        ChangeSpeed speed ->
            { model | speed = speed }
                |> withoutCmd

        ChangeZoom zoom ->
            { model | zoom = zoom }
                |> withoutCmd

        ChangeTheme theme ->
            { model | theme = theme }
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
                Nothing ->
                    { model | importField = Open userInput }
                        |> withoutCmd

                Just importedPattern ->
                    { model | importField = Closed, zoom = Far }
                        |> displayPattern WithPadding importedPattern
                        |> withoutCmd

        ImportFieldCancel ->
            { model | importField = Closed }
                |> withoutCmd

        RandomPatternRequest ->
            ( model, requestRandomPattern )

        RandomPatternResponse randomPattern ->
            displayPattern WithoutPadding randomPattern model
                |> withoutCmd

        NoOp ->
            withoutCmd model



-- UPDATE HELPERS


withoutCmd : Model -> ( Model, Cmd msg )
withoutCmd model =
    ( model, Cmd.none )


pauseGame : Model -> Model
pauseGame model =
    { model | status = Paused }


displayPattern : Padding -> Pattern -> Model -> Model
displayPattern padding pattern model =
    let
        newGame =
            GameOfLife.beginWithPattern padding pattern
    in
    History.record (always newGame) model.game
        |> setGame model


toggleCell : Coordinate -> Model -> Model
toggleCell coordinate model =
    History.record (GameOfLife.toggleCell coordinate) model.game
        |> setGame model


stepGame : Model -> Model
stepGame model =
    History.record GameOfLife.step model.game
        |> setGame model


tryUndoStep : Model -> Maybe Model
tryUndoStep model =
    History.undo model.game
        |> Maybe.map (setGame model)


tryRedoStep : Model -> Maybe Model
tryRedoStep model =
    History.redo model.game
        |> Maybe.map (setGame model)


setGame : Model -> History GameOfLife -> Model
setGame model game =
    { model | game = game }


ifGameFinished : (Model -> Model) -> Model -> Model
ifGameFinished updateModel model =
    if History.isUnchanged model.game then
        updateModel model

    else
        model


requestRandomPattern : Cmd Msg
requestRandomPattern =
    Pattern.generator defaultGameSize
        |> Random.generate RandomPatternResponse



-- VIEW


document : Model -> Document Msg
document model =
    { title = "Game of Life"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    div
        []
        [ bodyStyles model.theme
        , viewGame model
        , viewControls model
        ]


bodyStyles : Theme -> Html msg
bodyStyles theme =
    let
        backgroundColorStyle =
            "body { background-color: " ++ backgroundColor theme ++ "; }"
    in
    node "style"
        []
        [ text backgroundColorStyle ]


backgroundColor : Theme -> String
backgroundColor theme =
    case theme of
        Light ->
            "white"

        Dark ->
            "rgb(15, 15, 15)"


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
        model.speed
        model.zoom
        model.theme
        model.importField
        (controlEventHandlers model)


gameEventHandlers : GameOfLife.Events Msg
gameEventHandlers =
    { onMouseOver = MouseOver
    , onMouseDown = MouseDown
    , onMouseUp = MouseUp
    }


controlEventHandlers : Model -> Controls.Events Msg
controlEventHandlers { speed, zoom, theme, status } =
    { onStepBack = StepBack
    , onStepForward = StepForward
    , onSpeedChange = ChangeSpeed (nextSpeed speed)
    , onZoomChange = ChangeZoom (nextZoomLevel zoom)
    , onThemeChange = ChangeTheme (nextTheme theme)
    , onStatusChange = ChangeStatus (nextStatus status)
    , onRandomize = RandomPatternRequest
    , onImportFieldOpen = ImportFieldOpen
    , onImportFieldChange = ImportFieldChange
    , onImportFieldCancel = ImportFieldCancel
    , noOp = NoOp
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ keyDownSubscription model
        , tickSubscription model.status model.speed
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


keyDownSubscription : Model -> Sub Msg
keyDownSubscription model =
    let
        keyDecoder =
            Decode.field "key" Decode.string

        onKeyDown =
            Controls.onKeyDown (controlEventHandlers model)
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
