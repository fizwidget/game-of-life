module Main exposing (main)

import Html
import Html.Styled exposing (Html, toUnstyled, div, span)
import Html.Styled.Attributes exposing (css)
import Css.Foreign exposing (global, body)
import Css exposing (..)
import Css.Colors as Colors
import Time as Time exposing (millisecond)
import Data.Matrix as Matrix exposing (Matrix, Coordinate)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update Tick model =
    ( { cells = step model.cells }, Cmd.none )


step : Cells -> Cells
step cells =
    Matrix.coordinateMap (updateCell cells) cells


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


view : Model -> Html msg
view model =
    div [] (globalStyles :: (viewCells model.cells))


globalStyles : Html msg
globalStyles =
    global [ body [ margin (px 0), backgroundColor Colors.black ] ]


viewCells : Cells -> List (Html msg)
viewCells =
    Matrix.getRows >> List.map viewRow


viewRow : List Cell -> Html msg
viewRow cells =
    div
        [ css [ lineHeight (px 0) ] ]
        (List.map (viewCell (cellSize cells)) cells)


viewCell : Float -> Cell -> Html msg
viewCell cellSize cell =
    div
        [ css
            [ width (vw cellSize)
            , height (vw cellSize)
            , backgroundColor (cellColor cell)
            , display inlineBlock
            ]
        ]
        []


cellSize : List Cell -> Float
cellSize cells =
    100.0 / toFloat (List.length cells)


cellColor : Cell -> Css.Color
cellColor cell =
    case cell of
        Alive ->
            Colors.white

        Dead ->
            Colors.black


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
