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
import Simulation exposing (Simulation, Speed(..), Zoom(..))
import Time



-- MODEL


type Status
    = Paused
    | Playing


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
    noCmd initialModel


initialModel : Model
initialModel =
    { status = Paused
    , simulation = History.begin Simulation.create
    , mouse = Up
    , speed = Slow
    , zoom = Far
    , importField = Closed
    }



-- UPDATE


type Msg
    = Play
    | Pause
    | Tick
    | Undo
    | Redo
    | SetSpeed Speed
    | SetZoom Zoom
    | MouseDown Coordinate
    | MouseOver Coordinate
    | MouseUp
    | ImportFieldOpen
    | ImportFieldChange UserInput
    | Randomize
    | RandomizationComplete Pattern
    | NoOp


type alias Coordinate =
    { x : Int
    , y : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            { model | status = Playing }
                |> noCmd

        Pause ->
            { model | status = Paused }
                |> noCmd

        Tick ->
            { model | simulation = History.record Simulation.step model.simulation }
                |> pauseIfUnchanged
                |> noCmd

        Undo ->
            undo model
                |> noCmd

        Redo ->
            redoOrStep model
                |> noCmd

        SetSpeed speed ->
            { model | speed = speed }
                |> noCmd

        SetZoom zoom ->
            { model | zoom = zoom }
                |> noCmd

        MouseDown coordinate ->
            noCmd
                { model
                    | mouse = Down
                    , simulation = toggleCell coordinate model.simulation
                }

        MouseOver coordinate ->
            case model.mouse of
                Down ->
                    { model | simulation = toggleCell coordinate model.simulation }
                        |> noCmd

                Up ->
                    model
                        |> noCmd

        MouseUp ->
            { model | mouse = Up }
                |> noCmd

        ImportFieldOpen ->
            { model | importField = Open "" }
                |> noCmd

        ImportFieldChange text ->
            case parsePattern text of
                Nothing ->
                    { model | importField = Open text }
                        |> noCmd

                Just newSimulation ->
                    noCmd
                        { model
                            | importField = Closed
                            , simulation = History.record (always newSimulation) model.simulation
                            , zoom = Normal
                        }

        Randomize ->
            model
                |> withCmd randomizePattern

        RandomizationComplete randomPattern ->
            let
                randomSimulation =
                    Simulation.createWithPattern randomPattern
            in
            { model | simulation = History.record (always randomSimulation) model.simulation }
                |> noCmd

        NoOp ->
            noCmd model


noCmd : Model -> ( Model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
    ( model, cmd )


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
        |> Maybe.map Simulation.createWithPattern


randomizePattern : Cmd Msg
randomizePattern =
    Random.generate RandomizationComplete Pattern.generator



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

        transitionDuration =
            calculateTransitionDuration speed

        -- Refactor mouse stuff to keep `isSelectionActive` in model, then
        -- determine whether to dispatch `ToggleCell` or `NoOp` here.
        handlers =
            { mouseOver = MouseOver
            , mouseDown = MouseDown
            , mouseUp = MouseUp
            }
    in
    div
        [ class "center-content" ]
        [ Simulation.view transitionDuration currentSimulation zoom handlers
        , viewControls status speed zoom currentSimulation importField
        ]


calculateTransitionDuration : Speed -> Milliseconds
calculateTransitionDuration speed =
    tickInterval speed + 200


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
            viewButton "Play" Play []

        ( Paused, False ) ->
            viewButton "Play" Play [ class "green-button" ]

        ( Playing, _ ) ->
            viewButton "Pause" Pause []


viewSpeedButton : Speed -> Html Msg
viewSpeedButton speed =
    let
        onClick =
            SetSpeed (nextSpeed speed)
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
            SetZoom (nextZoomLevel zoom)
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
    viewButton "Random" Randomize []


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
            Time.every (tickInterval speed) (always Tick)

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
            case status of
                Playing ->
                    Pause

                Paused ->
                    Play

        "s" ->
            SetSpeed (nextSpeed speed)

        "r" ->
            Randomize

        "z" ->
            SetZoom (nextZoomLevel zoom)

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
