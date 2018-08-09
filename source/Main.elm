module Main exposing (main)

import Html
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (Html, toUnstyled, div, span, button, text)
import Html.Styled.Attributes exposing (css, class)
import Css.Foreign exposing (global, body)
import Css exposing (..)
import Css.Colors as Colors
import Time as Time exposing (millisecond)
import Matrix as Matrix exposing (Matrix, Coordinate)


-- MODEL


type Cell
    = Alive
    | Dead


type alias Cells =
    Matrix Cell


type Status
    = Paused
    | Playing


type alias Model =
    { cells : Cells, status : Status }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { cells = line, status = Paused }
    , Cmd.none
    )


emptyMatrix : Cells
emptyMatrix =
    Matrix.create { width = 30, height = 30 } Dead


line : Cells
line =
    Matrix.create { width = 20, height = 20 } Dead
        |> Matrix.set { x = 7, y = 6 } Alive
        |> Matrix.set { x = 8, y = 6 } Alive
        |> Matrix.set { x = 9, y = 6 } Alive
        |> Matrix.set { x = 10, y = 6 } Alive
        |> Matrix.set { x = 11, y = 6 } Alive
        |> Matrix.set { x = 12, y = 6 } Alive
        |> Matrix.set { x = 13, y = 6 } Alive
        |> Matrix.set { x = 14, y = 6 } Alive
        |> Matrix.set { x = 15, y = 6 } Alive


oddPattern : Cells
oddPattern =
    Matrix.create { width = 20, height = 20 } Dead
        |> Matrix.set { x = 3, y = 3 } Alive
        |> Matrix.set { x = 3, y = 4 } Alive
        |> Matrix.set { x = 3, y = 5 } Alive
        |> Matrix.set { x = 2, y = 5 } Alive
        |> Matrix.set { x = 6, y = 4 } Alive
        |> Matrix.set { x = 8, y = 3 } Alive
        |> Matrix.set { x = 4, y = 7 } Alive
        |> Matrix.set { x = 4, y = 10 } Alive
        |> Matrix.set { x = 7, y = 9 } Alive
        |> Matrix.set { x = 8, y = 8 } Alive



-- UPDATE


type Msg
    = Tick
    | Toggle Coordinate
    | Play
    | Pause


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
            { model | cells = step model.cells }
                |> noCmd

        Toggle coordinate ->
            { model | cells = toggle model.cells coordinate }
                |> noCmd


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


toggle : Cells -> Coordinate -> Cells
toggle cells coordinate =
    Matrix.update coordinate cells toggleCell


toggleCell : Cell -> Cell
toggleCell cell =
    case cell of
        Alive ->
            Dead

        Dead ->
            Alive


step : Cells -> Cells
step cells =
    Matrix.indexedMap (updateCell cells) cells


updateCell : Cells -> Coordinate -> Cell -> Cell
updateCell cells coordinate cell =
    case ( cell, countNeighbours cells coordinate ) of
        ( Alive, 2 ) ->
            Alive

        ( Alive, 3 ) ->
            Alive

        ( Dead, 3 ) ->
            Alive

        _ ->
            Dead


countNeighbours : Cells -> Coordinate -> Int
countNeighbours cells coordinate =
    let
        countLiveCell cell currentCount =
            case cell of
                Alive ->
                    currentCount + 1

                Dead ->
                    currentCount
    in
        Matrix.getNeighbours coordinate cells
            |> List.foldl countLiveCell 0



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ globalStyles, viewModel model ]


globalStyles : Html msg
globalStyles =
    global [ body [ margin (px 0), backgroundColor Colors.white ] ]


viewModel : Model -> Html Msg
viewModel { cells, status } =
    div
        []
        [ viewCells cells, viewPlayPauseButton status ]


viewPlayPauseButton : Status -> Html Msg
viewPlayPauseButton status =
    let
        styles =
            css
                [ position fixed
                , width (px 100)
                , height (px 40)
                , marginLeft auto
                , marginRight auto
                , left (px 0)
                , right (px 0)
                , bottom (pct 6)
                , backgroundColor (rgba 76 154 255 0.9)
                , border2 (px 0) none
                , borderRadius (px 10)
                , color Colors.white
                , fontSize (px 20)
                ]
    in
        case status of
            Playing ->
                button [ onClick Pause, styles ] [ text "Pause" ]

            Paused ->
                button [ onClick Play, styles ] [ text "Play" ]


viewCells : Cells -> Html Msg
viewCells cells =
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , flexWrap wrap
            ]
        ]
        (Matrix.toListWithCoordinates cells
            |> List.map (viewCell (cellSize cells))
        )


viewCell : Float -> ( Coordinate, Cell ) -> Html Msg
viewCell size ( coordinate, cell ) =
    div
        [ class "cell"
        , css
            [ width (vw size)
            , height (vh size)
            , backgroundColor (cellColor cell)
            , displayFlex
            , flex3 (int 0) (int 0) (pct size)
            , borderRadius (pct 50)
            , border3 (px 4) solid Colors.white
            , boxSizing borderBox
            ]
        , (onClick (Toggle coordinate))
        ]
        []


cellSize : Cells -> Float
cellSize cells =
    100.0 / toFloat (Matrix.width cells)


cellColor : Cell -> Css.Color
cellColor cell =
    case cell of
        Alive ->
            rgba 38 132 255 0.8

        Dead ->
            Colors.white



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { status } =
    case status of
        Playing ->
            Time.every (millisecond * 200) (always Tick)

        Paused ->
            Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
