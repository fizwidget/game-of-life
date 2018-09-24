module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import Cell exposing (Cell(..))
import Css exposing (..)
import Css.Transitions as Transitions exposing (easeInOut, transition)
import History exposing (History)
import Html.Styled as Html exposing (Html, button, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, onMouseDown, onMouseEnter, onMouseUp)
import Import
import Json.Decode as Decode exposing (Decoder)
import Matrix exposing (Coordinate, Matrix)
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


type alias Cells =
    Matrix Cell


type Importer
    = Open Import.Model
    | Closed


type alias Model =
    { status : Status
    , cells : History Cells
    , mouse : Mouse
    , speed : Speed
    , importer : Importer
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { status = Paused
      , cells = History.begin initialCells
      , mouse = Up
      , speed = Slow
      , importer = Closed
      }
    , Cmd.none
    )


initialCells : Cells
initialCells =
    Matrix.create { width = 18, height = 18 } Dead



-- UPDATE


type Msg
    = Play
    | Pause
    | Tick
    | Undo
    | Redo
    | SetSpeed Speed
    | MouseDown Coordinate
    | MouseUp
    | MouseOver Coordinate
    | KeyDown Key
    | OpenImporter
    | ImportMsg Import.Msg


type Key
    = LeftKey
    | RightKey
    | PKey
    | OtherKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg ({ status, mouse, cells, importer } as model) =
    case msg of
        Play ->
            { model | status = Playing }

        Pause ->
            { model | status = Paused }

        Tick ->
            { model | cells = History.record step cells }
                |> pauseIfSettled

        Undo ->
            undo model

        Redo ->
            redo model

        SetSpeed speed ->
            { model | speed = speed }

        MouseDown coordinate ->
            { model
                | mouse = Down
                , cells = History.record (toggleCoordinate coordinate) cells
            }

        MouseUp ->
            { model | mouse = Up }

        MouseOver coordinate ->
            case mouse of
                Up ->
                    model

                Down ->
                    { model | cells = History.record (toggleCoordinate coordinate) cells }

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

        OpenImporter ->
            { model | importer = Open Import.init }

        ImportMsg importMsg ->
            case importer of
                Open importerModel ->
                    let
                        ( updatedImporterModel, outMsg ) =
                            Import.update importMsg importerModel
                    in
                    case outMsg of
                        Import.ImportConfirmed config ->
                            { model | cells = History.begin config, importer = Closed }

                        Import.NoOp ->
                            { model | importer = Open updatedImporterModel }

                Closed ->
                    model


undo : Model -> Model
undo model =
    { model
        | status = Paused
        , cells = History.undo model.cells
    }


redo : Model -> Model
redo ({ cells } as model) =
    { model
        | status = Paused
        , cells = History.redo cells |> Maybe.withDefault (History.record step cells)
    }


toggleStatus : Model -> Model
toggleStatus model =
    case model.status of
        Playing ->
            { model | status = Paused }

        Paused ->
            { model | status = Playing }


pauseIfSettled : Model -> Model
pauseIfSettled ({ status, cells } as model) =
    if History.didChange cells then
        model

    else
        { model | status = Paused }


step : Cells -> Cells
step cells =
    Matrix.coordinateMap (updateCell cells) cells


updateCell : Cells -> Coordinate -> Cell -> Cell
updateCell cells coordinate cell =
    case ( cell, liveNeighbours cells coordinate ) of
        ( Alive, 2 ) ->
            Alive

        ( Alive, 3 ) ->
            Alive

        ( Dead, 3 ) ->
            Alive

        _ ->
            Dead


liveNeighbours : Cells -> Coordinate -> Int
liveNeighbours cells coordinate =
    Matrix.neighbours cells coordinate
        |> List.filter ((==) Alive)
        |> List.length


toggleCoordinate : Coordinate -> Cells -> Cells
toggleCoordinate coordinate cells =
    Matrix.update toggleCell coordinate cells


toggleCell : Cell -> Cell
toggleCell cell =
    case cell of
        Alive ->
            Dead

        Dead ->
            Alive



-- VIEW


document : Model -> Document Msg
document model =
    { title = "Game of Life"
    , body = [ view model |> toUnstyled ]
    }


view : Model -> Html Msg
view { cells, status, speed, importer } =
    let
        currentCells =
            History.now cells

        transitionDuration =
            calculateTransitionDuration speed
    in
    div
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]
        [ squareContainer <| viewCells transitionDuration currentCells
        , viewControls status speed currentCells importer
        ]


squareContainer : Html msg -> Html msg
squareContainer content =
    div
        [ css
            [ position relative
            , width (pct 100)
            , after
                [ property "content" "''"
                , display block
                , paddingBottom (pct 100)
                ]
            ]
        ]
        [ content ]


viewCells : Milliseconds -> Cells -> Html Msg
viewCells transitionDuration cells =
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , flexWrap wrap
            , position absolute
            , width (pct 100)
            , height (pct 100)
            ]
        ]
        (cells
            |> Matrix.coordinateMap (viewCell transitionDuration (cellSize cells))
            |> Matrix.toList
        )


viewCell : Milliseconds -> Percentage -> Coordinate -> Cell -> Html Msg
viewCell transitionDuration size coordinate cell =
    div
        [ css
            [ width (pct size)
            , height (pct size)
            , displayFlex
            , justifyContent center
            , alignItems center
            ]
        , onMouseDown (MouseDown coordinate)
        , onMouseUp MouseUp
        , onMouseEnter (MouseOver coordinate)
        ]
        [ viewCellContent transitionDuration cell coordinate ]


viewCellContent : Milliseconds -> Cell -> Coordinate -> Html msg
viewCellContent transitionDuration cell coordinate =
    div
        [ css
            [ width (pct (cellContentSize cell))
            , height (pct (cellContentSize cell))
            , backgroundColor (cellColor cell coordinate)
            , borderRadius (pct 30)
            , transition
                [ Transitions.backgroundColor3 transitionDuration 0 easeInOut
                , Transitions.width transitionDuration
                , Transitions.height transitionDuration
                ]
            ]
        ]
        []


calculateTransitionDuration : Speed -> Milliseconds
calculateTransitionDuration speed =
    tickInterval speed + 200


cellSize : Cells -> Percentage
cellSize cells =
    100.0 / (Matrix.height cells |> toFloat)


cellContentSize : Cell -> Percentage
cellContentSize cell =
    case cell of
        Alive ->
            70

        Dead ->
            40


type alias Percentage =
    Float


cellColor : Cell -> Coordinate -> Color
cellColor cell { x, y } =
    case cell of
        Dead ->
            rgb 244 245 247

        Alive ->
            case ( modBy 2 x == 0, modBy 2 y == 0 ) of
                ( True, True ) ->
                    rgba 255 171 0 0.8

                ( True, False ) ->
                    rgba 54 179 126 0.8

                ( False, True ) ->
                    rgba 0 184 217 0.8

                ( False, False ) ->
                    rgba 101 84 192 0.8


viewControls : Status -> Speed -> Cells -> Importer -> Html Msg
viewControls status speed cells importer =
    div []
        [ bottomLeft
            [ viewImporter importer
            , viewStatusButton status |> ifNotBlank cells
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


ifNotBlank : Cells -> Html msg -> Html msg
ifNotBlank cells children =
    if Matrix.all ((==) Dead) cells then
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


viewImporter : Importer -> Html Msg
viewImporter loader =
    case loader of
        Open model ->
            Html.map ImportMsg (Import.view model)

        Closed ->
            viewButton "Import" OpenImporter []


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
            Time.every (tickInterval speed) (always Tick)

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
