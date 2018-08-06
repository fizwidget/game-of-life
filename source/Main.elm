module Main exposing (main)

import Html
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (Html, toUnstyled, div, span, button)
import Html.Styled.Attributes exposing (css)
import Css.Foreign exposing (global, body)
import Css exposing (..)
import Css.Colors as Colors
import Time as Time exposing (millisecond)
import Matrix as Matrix exposing (Matrix, Coordinate)


-- Model


type Cell
    = Alive
    | Dead


type alias Cells =
    Matrix Cell


type alias Model =
    { cells : Cells }



-- Init


init : ( Model, Cmd Msg )
init =
    ( { cells = initialCells }, Cmd.none )


initialCells : Cells
initialCells =
    Matrix.create { width = 30, height = 30 } Dead
        |> Matrix.set { x = 15 + 3, y = 15 + 3 } Alive
        |> Matrix.set { x = 15 + 3, y = 15 + 4 } Alive
        |> Matrix.set { x = 15 + 3, y = 15 + 5 } Alive
        |> Matrix.set { x = 15 + 2, y = 15 + 5 } Alive
        |> Matrix.set { x = 15 + 6, y = 15 + 4 } Alive
        |> Matrix.set { x = 15 + 8, y = 15 + 3 } Alive
        |> Matrix.set { x = 15 + 4, y = 15 + 7 } Alive
        |> Matrix.set { x = 15 + 4, y = 15 + 10 } Alive
        |> Matrix.set { x = 15 + 7, y = 15 + 9 } Alive
        |> Matrix.set { x = 15 + 8, y = 15 + 8 } Alive



-- Update


type Msg
    = Tick
    | Toggle Coordinate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { cells = step model.cells }, Cmd.none )

        Toggle coordinate ->
            Matrix.get model.cells coordinate
                |> Maybe.map toggle
                |> Maybe.map (\cell -> Matrix.set coordinate cell model.cells)
                |> Maybe.map (\cells -> { cells = cells })
                |> Maybe.withDefault model
                |> (\model -> ( model, Cmd.none ))


toggle : Cell -> Cell
toggle cell =
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



-- View


view : Model -> Html Msg
view model =
    div [] [ globalStyles, viewModel model ]


globalStyles : Html msg
globalStyles =
    global [ body [ margin (px 0), backgroundColor Colors.black ] ]


viewModel : Model -> Html Msg
viewModel { cells } =
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
        [ css
            [ width (vh size)
            , height (vh size)
            , backgroundColor (cellColor cell)
            , displayFlex
            , flex3 (int 0) (int 0) (pct size)
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
            Colors.black

        Dead ->
            Colors.white


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (millisecond * 150) (always Tick)



-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
