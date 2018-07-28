module Main exposing (main)

import Html
import Html.Styled exposing (Html, toUnstyled, div, span)
import Html.Styled.Attributes exposing (css)
import Css.Foreign exposing (global, body)
import Css exposing (..)
import Css.Colors as Colors
import Time as Time exposing (millisecond)
import Data.Matrix as Matrix exposing (Matrix, Coordinate)


type Msg
    = Tick


type Cell
    = Alive
    | Dead


type alias Model =
    { matrix : Matrix Cell
    }


init : ( Model, Cmd Msg )
init =
    ( { matrix = initialMatrix }, Cmd.none )


initialMatrix : Matrix Cell
initialMatrix =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { matrix = iterate model.matrix }, Cmd.none )


iterate : Matrix Cell -> Matrix Cell
iterate matrix =
    Matrix.indexedMap (updateCell matrix) matrix


updateCell : Matrix Cell -> Coordinate -> Cell -> Cell
updateCell matrix coordinate cell =
    let
        neighbourCount =
            Matrix.getNeighbours coordinate matrix
                |> List.foldl countLiveCells 0
    in
        case ( cell, neighbourCount ) of
            ( Alive, 2 ) ->
                Alive

            ( Alive, 3 ) ->
                Alive

            ( Dead, 3 ) ->
                Alive

            _ ->
                Dead


countLiveCells : Cell -> Int -> Int
countLiveCells cell currentCount =
    case cell of
        Alive ->
            currentCount + 1

        Dead ->
            currentCount


view : Model -> Html msg
view model =
    let
        globalStyles =
            global [ body [ margin (px 0), backgroundColor Colors.black ] ]

        modelView =
            model.matrix
                |> Matrix.getRows
                |> List.map viewRow
    in
        div [] (globalStyles :: modelView)


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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
