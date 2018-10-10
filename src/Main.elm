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
import Time
import World exposing (World)



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
    = Open UserInput
    | Closed


type alias UserInput =
    String


type alias Model =
    { world : History World
    , status : Status
    , mouse : Mouse
    , speed : Speed
    , zoom : World.Zoom
    , importField : ImportField
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    noCmd initialModel


initialModel : Model
initialModel =
    { status = Paused
    , world = History.begin World.create
    , mouse = Up
    , speed = Slow
    , zoom = World.Far
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
    | SetZoom World.Zoom
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
            { model | world = History.record World.step model.world }
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
                    , world = toggleCell coordinate model.world
                }

        MouseOver coordinate ->
            case model.mouse of
                Down ->
                    { model | world = toggleCell coordinate model.world }
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

                Just newWorld ->
                    noCmd
                        { model
                            | importField = Closed
                            , world = History.record (always newWorld) model.world
                            , zoom = World.Normal
                        }

        Randomize ->
            model
                |> withCmd randomizePattern

        RandomizationComplete randomPattern ->
            let
                randomWorld =
                    World.createWithPattern randomPattern
            in
            { model | world = History.record (always randomWorld) model.world }
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
        , world =
            History.undo model.world
                |> Maybe.withDefault model.world
    }


redoOrStep : Model -> Model
redoOrStep model =
    { model
        | status = Paused
        , world =
            History.redo model.world
                |> Maybe.withDefault (History.record World.step model.world)
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
    if History.isUnchanged model.world then
        { model | status = Paused }

    else
        model


toggleCell : Coordinate -> History World -> History World
toggleCell coordinate world =
    History.record (World.toggleCell coordinate) world


parsePattern : String -> Maybe World
parsePattern text =
    Pattern.parseLife106 text
        |> Maybe.map World.createWithPattern


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
view { world, status, speed, zoom, importField } =
    let
        currentWorld =
            History.now world

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
        [ World.view transitionDuration currentWorld zoom handlers
        , viewControls status speed zoom currentWorld importField
        ]


calculateTransitionDuration : Speed -> Milliseconds
calculateTransitionDuration speed =
    tickInterval speed + 200


viewControls : Status -> Speed -> World.Zoom -> World -> ImportField -> Html Msg
viewControls status speed zoom world importField =
    div []
        [ div [ class "bottom-left-overlay" ]
            [ viewStatusButton status world
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


viewStatusButton : Status -> World -> Html Msg
viewStatusButton status world =
    case ( status, World.isFinished world ) of
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


viewZoomButton : World.Zoom -> Html Msg
viewZoomButton zoom =
    let
        onClick =
            SetZoom (nextZoomLevel zoom)
    in
    case zoom of
        World.Far ->
            viewButton "1X" onClick []

        World.Normal ->
            viewButton "1.5X" onClick []

        World.Close ->
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


nextZoomLevel : World.Zoom -> World.Zoom
nextZoomLevel zoom =
    case zoom of
        World.Far ->
            World.Normal

        World.Normal ->
            World.Close

        World.Close ->
            World.Far



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


keyDownSubscription : Status -> Speed -> World.Zoom -> Sub Msg
keyDownSubscription status speed zoom =
    Events.onKeyDown (keyDecoder status speed zoom)


keyDecoder : Status -> Speed -> World.Zoom -> Decoder Msg
keyDecoder status speed zoom =
    Decode.field "key" Decode.string
        |> Decode.map (toMsg status speed zoom)


toMsg : Status -> Speed -> World.Zoom -> String -> Msg
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
