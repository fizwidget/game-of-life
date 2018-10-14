module GameOfLife exposing
    ( Cell(..)
    , Events
    , GameOfLife
    , Padding(..)
    , begin
    , beginWithPattern
    , isFinished
    , step
    , toggleCell
    , view
    )

import Common exposing (Theme(..), Zoom(..))
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onMouseDown, onMouseEnter, onMouseUp)
import Matrix exposing (Coordinate, Matrix)
import Pattern exposing (Pattern)



-- TYPES


type Cell
    = Alive
    | Dead


type alias Cells =
    Matrix Cell


type GameOfLife
    = GameOfLife Cells


type alias Dimensions =
    { width : Int
    , height : Int
    }


type Padding
    = WithPadding
    | NoPadding



-- CREATE


begin : Dimensions -> GameOfLife
begin dimensions =
    Matrix.create dimensions Dead
        |> GameOfLife


beginWithPattern : Padding -> Pattern -> GameOfLife
beginWithPattern padding pattern =
    let
        paddingAmount =
            case padding of
                NoPadding ->
                    0

                WithPadding ->
                    6

        size =
            max (Pattern.width pattern) (Pattern.height pattern)
                |> (+) paddingAmount
                |> max 18

        center =
            { x = size // 2
            , y = size // 2
            }

        centeredPattern =
            Pattern.centerAt center pattern

        deadCells =
            Matrix.create { width = size, height = size } Dead
    in
    GameOfLife <|
        List.foldl
            (Matrix.set Alive)
            deadCells
            (Pattern.toCoordinates centeredPattern)



-- OPERATIONS


step : GameOfLife -> GameOfLife
step (GameOfLife cells) =
    Matrix.coordinateMap (stepCell cells) cells
        |> GameOfLife


stepCell : Cells -> Coordinate -> Cell -> Cell
stepCell cells coordinate cell =
    case ( cell, countLiveNeighbours cells coordinate ) of
        ( Alive, 2 ) ->
            Alive

        ( Alive, 3 ) ->
            Alive

        ( Dead, 3 ) ->
            Alive

        _ ->
            Dead


countLiveNeighbours : Cells -> Coordinate -> Int
countLiveNeighbours cells coordinate =
    Matrix.neighbours cells coordinate
        |> List.filter ((==) Alive)
        |> List.length


toggleCell : Coordinate -> GameOfLife -> GameOfLife
toggleCell coordinate (GameOfLife cells) =
    let
        toggle cell =
            case cell of
                Alive ->
                    Dead

                Dead ->
                    Alive
    in
    Matrix.update toggle coordinate cells
        |> GameOfLife


isFinished : GameOfLife -> Bool
isFinished (GameOfLife cells) =
    Matrix.all ((==) Dead) cells



-- VIEW


type alias Percentage =
    Float


type alias Events msg =
    { onMouseOver : Coordinate -> msg
    , onMouseDown : Coordinate -> msg
    , onMouseUp : msg
    }


view : GameOfLife -> Zoom -> Theme -> Events msg -> Html msg
view game zoom theme events =
    div
        [ class "square-container" ]
        [ viewCells game zoom theme events ]


viewCells : GameOfLife -> Zoom -> Theme -> Events msg -> Html msg
viewCells (GameOfLife cells) zoom theme events =
    div
        ([ class "cells-container" ] ++ zoomStyles zoom)
        (cells
            |> Matrix.coordinateMap (viewCell (cellSize cells) theme events)
            |> Matrix.toList
        )


zoomStyles : Zoom -> List (Attribute msg)
zoomStyles zoom =
    let
        percentage =
            case zoom of
                Far ->
                    percentageStyle 100

                Normal ->
                    percentageStyle 150

                Close ->
                    percentageStyle 200
    in
    [ style "width" percentage
    , style "height" percentage
    ]


viewCell : Percentage -> Theme -> Events msg -> Coordinate -> Cell -> Html msg
viewCell relativeSize theme events coordinate cell =
    div
        [ class "center-content"
        , style "width" (percentageStyle relativeSize)
        , style "height" (percentageStyle relativeSize)
        , onMouseDown (events.onMouseDown coordinate)
        , onMouseUp events.onMouseUp
        , onMouseEnter (events.onMouseOver coordinate)
        ]
        [ viewCellContent cell coordinate theme ]


viewCellContent : Cell -> Coordinate -> Theme -> Html msg
viewCellContent cell coordinate theme =
    let
        size =
            cellContentSize cell
    in
    div
        [ class "cell-content"
        , class (cellColorClass cell coordinate theme)
        , style "width" (percentageStyle size)
        , style "height" (percentageStyle size)
        ]
        []


percentageStyle : Percentage -> String
percentageStyle value =
    String.fromFloat value ++ "%"


cellSize : Cells -> Percentage
cellSize cells =
    100.0 / (Matrix.width cells |> toFloat)


cellContentSize : Cell -> Percentage
cellContentSize cell =
    case cell of
        Alive ->
            70

        Dead ->
            40


cellColorClass : Cell -> Coordinate -> Theme -> String
cellColorClass cell { x, y } theme =
    case cell of
        Dead ->
            case theme of
                Light ->
                    "light-grey-background"

                Dark ->
                    "dark-grey-background"

        Alive ->
            case ( modBy 2 x == 0, modBy 2 y == 0 ) of
                ( True, True ) ->
                    "orange-background"

                ( True, False ) ->
                    "green-background"

                ( False, True ) ->
                    "blue-background"

                ( False, False ) ->
                    "purple-background"
