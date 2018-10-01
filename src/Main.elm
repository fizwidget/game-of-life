module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import Button
import Css exposing (..)
import History exposing (History)
import Html.Styled as Html exposing (Html, div, textarea, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, cols, css, placeholder, rows, value)
import Html.Styled.Events exposing (onInput)
import Json.Decode as Decode exposing (Decoder)
import Overlay
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
    | Fast


type ImportField
    = Open String
    | Closed


type alias Model =
    { world : History World
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
    , world = History.begin World.empty
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
            { model | world = History.record World.step model.world }
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
        |> Maybe.map World.withPattern



-- VIEW


document : Model -> Document Msg
document model =
    { title = "Game of Life"
    , body = [ view model |> toUnstyled ]
    }


view : Model -> Html Msg
view { world, status, speed, importField } =
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
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]
        [ World.view transitionDuration currentWorld handlers
        , viewControls status speed currentWorld importField
        ]


calculateTransitionDuration : Speed -> Milliseconds
calculateTransitionDuration speed =
    tickInterval speed + 200


viewControls : Status -> Speed -> World -> ImportField -> Html Msg
viewControls status speed world importField =
    Overlay.view
        { bottomLeft =
            [ viewImportField importField
            , viewStatusButton status |> hideIfFinished world
            , viewSpeedButton speed
            ]
        , bottomRight =
            [ viewUndoButton status
            , viewRedoButton status
            ]
        }


hideIfFinished : World -> Html msg -> Html msg
hideIfFinished world children =
    if World.isFinished world then
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
