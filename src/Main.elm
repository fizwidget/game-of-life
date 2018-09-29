module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import Css exposing (..)
import Css.Transitions as Transitions exposing (easeInOut, transition)
import History exposing (History)
import Html.Styled as Html exposing (Html, button, div, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, cols, css, disabled, placeholder, rows, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra as Maybe
import Pattern exposing (Pattern)
import Simulation exposing (Cell(..), Cells)
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
    | Fast


type ImportField
    = Open String
    | Closed


type alias Model =
    { status : Status
    , cells : History Cells
    , mouse : Mouse
    , speed : Speed
    , importField : ImportField
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { status = Paused
      , cells = History.begin Simulation.begin
      , mouse = Up
      , speed = Slow
      , importField = Closed
      }
    , Cmd.none
    )



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
    | OpenImportField
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
            { model | cells = History.record Simulation.step model.cells }
                |> pauseIfStable

        Undo ->
            undo model

        Redo ->
            redo model

        SetSpeed speed ->
            { model | speed = speed }

        MouseDown coordinate ->
            { model
                | mouse = Down
                , cells = History.record (Simulation.toggleCoordinate coordinate) model.cells
            }

        MouseUp ->
            { model | mouse = Up }

        MouseOver coordinate ->
            case model.mouse of
                Down ->
                    { model | cells = History.record (Simulation.toggleCoordinate coordinate) model.cells }

                Up ->
                    model

        KeyDown key ->
            case key of
                LeftKey ->
                    undo model

                RightKey ->
                    redo model

                PKey ->
                    toggleStatus model

                OtherKey ->
                    model

        OpenImportField ->
            { model | importField = Open "" }

        ImportFieldChange text ->
            case parseCells text of
                Nothing ->
                    { model | importField = Open text }

                Just parsedCells ->
                    { model
                        | importField = Closed
                        , cells = History.record (always parsedCells) model.cells
                    }


undo : Model -> Model
undo model =
    { model
        | status = Paused
        , cells = History.undo model.cells
    }


redo : Model -> Model
redo model =
    { model
        | status = Paused
        , cells =
            History.redo model.cells
                |> Maybe.withDefault (History.record Simulation.step model.cells)
    }


toggleStatus : Model -> Model
toggleStatus model =
    case model.status of
        Playing ->
            { model | status = Paused }

        Paused ->
            { model | status = Playing }


pauseIfStable : Model -> Model
pauseIfStable model =
    if History.isStable model.cells then
        { model | status = Paused }

    else
        model


parseCells : String -> Maybe Cells
parseCells text =
    Pattern.parseLife106 text
        |> Maybe.map Simulation.beginWithPattern



-- VIEW


document : Model -> Document Msg
document model =
    { title = "Game of Life"
    , body = [ view model |> toUnstyled ]
    }


view : Model -> Html Msg
view { cells, status, speed, importField } =
    let
        currentCells =
            History.now cells

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
        [ Simulation.view transitionDuration currentCells handlers
        , viewControls status speed currentCells importField
        ]


calculateTransitionDuration : Speed -> Milliseconds
calculateTransitionDuration speed =
    tickInterval speed + 200


viewControls : Status -> Speed -> Cells -> ImportField -> Html Msg
viewControls status speed cells importField =
    div []
        [ bottomLeft
            [ viewImportField importField
            , viewStatusButton status |> unlessSimulationFinished cells
            , viewSpeedButton speed
            ]
        , bottomRight
            [ viewUndoButton status
            , viewRedoButton status
            ]
        ]


bottomLeft : List (Html msg) -> Html msg
bottomLeft =
    div
        [ css
            [ position fixed
            , left (px 20)
            , bottom (px 20)
            , displayFlex
            , flexDirection column
            ]
        ]


bottomRight : List (Html msg) -> Html msg
bottomRight =
    div
        [ css
            [ position fixed
            , right (px 20)
            , bottom (px 20)
            ]
        ]


unlessSimulationFinished : Cells -> Html msg -> Html msg
unlessSimulationFinished cells children =
    if Simulation.isFinished cells then
        div [] []

    else
        children


viewStatusButton : Status -> Html Msg
viewStatusButton status =
    case status of
        Playing ->
            viewButton "Pause" Pause []

        Paused ->
            viewButton "Play" Play [ backgroundColor (rgba 54 179 126 0.8) ]


viewSpeedButton : Speed -> Html Msg
viewSpeedButton speed =
    case speed of
        Slow ->
            viewButton "Faster" (SetSpeed Fast) []

        Fast ->
            viewButton "Slower" (SetSpeed Slow) []


viewImportField : ImportField -> Html Msg
viewImportField importField =
    case importField of
        Open input ->
            textarea
                [ rows 32
                , cols 30
                , autofocus True
                , placeholder "Paste a 'Life 1.06' pattern here"
                , css [ borderRadius (px 4), resize none ]
                , value input
                , onInput ImportFieldChange
                ]
                []

        Closed ->
            viewButton "Import" OpenImportField []


viewUndoButton : Status -> Html Msg
viewUndoButton status =
    viewButton "⬅︎" Undo []


viewRedoButton : Status -> Html Msg
viewRedoButton status =
    viewButton "➡︎" Redo []


viewButton : String -> Msg -> List Style -> Html Msg
viewButton description clickMsg styles =
    button
        [ onClick clickMsg, css (buttonStyles ++ styles) ]
        [ text description ]


buttonStyles : List Style
buttonStyles =
    [ width (px 100)
    , height (px 40)
    , margin (px 5)
    , border2 (px 0) none
    , borderRadius (px 15)
    , color (rgb 255 255 255)
    , backgroundColor (rgba 179 186 197 0.6)
    , fontSize (px 20)
    , transition
        [ Transitions.backgroundColor3 200 0 easeInOut
        , Transitions.visibility3 200 0 easeInOut
        ]
    ]



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
