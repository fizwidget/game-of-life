module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import History exposing (History)
import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (autofocus, class, cols, placeholder, rows, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Pattern exposing (Pattern)
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
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { status = Paused
    , world = History.begin World.create
    , mouse = Up
    , speed = Slow
    , zoom = World.Small
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
    | SetZoom World.Zoom
    | MouseDown Coordinate
    | MouseUp
    | MouseOver Coordinate
    | KeyDown Key
    | ImportFieldOpen
    | ImportFieldChange UserInput


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
            { model | world = History.record World.step model.world }
                |> pauseIfUnchanged

        Undo ->
            undo model

        Redo ->
            redoOrStep model

        SetSpeed speed ->
            { model | speed = speed }

        SetZoom zoom ->
            { model | zoom = zoom }

        MouseDown coordinate ->
            { model
                | mouse = Down
                , world = toggleCell coordinate model.world
            }

        MouseUp ->
            { model | mouse = Up }

        MouseOver coordinate ->
            case model.mouse of
                Down ->
                    { model | world = toggleCell coordinate model.world }

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

                Just newWorld ->
                    { model
                        | importField = Closed
                        , world = History.record (always newWorld) model.world
                        , zoom = World.Small
                    }


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
    case speed of
        Slow ->
            viewButton "Slow" (SetSpeed Medium) []

        Medium ->
            viewButton "Medium" (SetSpeed Fast) []

        Fast ->
            viewButton "Fast" (SetSpeed Slow) []


viewZoomButton : World.Zoom -> Html Msg
viewZoomButton zoom =
    case zoom of
        World.Small ->
            viewButton "1X" (SetZoom World.Medium) []

        World.Medium ->
            viewButton "1.5X" (SetZoom World.Large) []

        World.Large ->
            viewButton "2X" (SetZoom World.Small) []


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


viewButton : String -> msg -> List (Attribute msg) -> Html msg
viewButton description clickMsg customAttributes =
    let
        attributes =
            [ class "button", onClick clickMsg ] ++ customAttributes
    in
    button attributes [ text description ]



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
