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
    | WithoutPadding



-- CREATION


begin : Dimensions -> GameOfLife
begin dimensions =
    Matrix.create dimensions Dead
        |> GameOfLife


beginWithPattern : Padding -> Pattern -> GameOfLife
beginWithPattern padding pattern =
    let
        paddingCells =
            case padding of
                WithPadding ->
                    6

                WithoutPadding ->
                    0

        size =
            max (Pattern.width pattern) (Pattern.height pattern)
                |> (+) paddingCells
                |> max 18

        center =
            { x = size // 2
            , y = size // 2
            }

        centeredPattern =
            Pattern.centerAt center pattern
    in
    GameOfLife (initializeCells size centeredPattern)


initializeCells : Int -> Pattern -> Cells
initializeCells gameSize pattern =
    let
        makeAlive =
            Matrix.set Alive

        deadCells =
            Matrix.create { width = gameSize, height = gameSize } Dead

        patternCoordinates =
            Pattern.toCoordinates pattern
    in
    List.foldl makeAlive deadCells patternCoordinates



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


type alias ClassName =
    String


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
    let
        attributes =
            [ class "cells" ] ++ zoomStyles zoom
    in
    div
        attributes
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
                    percentString 100

                Normal ->
                    percentString 150

                Close ->
                    percentString 200
    in
    [ style "width" percentage
    , style "height" percentage
    ]


viewCell : Percentage -> Theme -> Events msg -> Coordinate -> Cell -> Html msg
viewCell relativeSize theme events coordinate cell =
    div
        [ class "cell"
        , style "width" (percentString relativeSize)
        , style "height" (percentString relativeSize)
        , onMouseDown (events.onMouseDown coordinate)
        , onMouseUp events.onMouseUp
        , onMouseEnter (events.onMouseOver coordinate)
        ]
        [ viewInnerCell cell coordinate theme ]


viewInnerCell : Cell -> Coordinate -> Theme -> Html msg
viewInnerCell cell coordinate theme =
    let
        size =
            innerCellSize cell
    in
    div
        [ class "inner-cell"
        , class (cellColorClass cell coordinate theme)
        , style "width" (percentString size)
        , style "height" (percentString size)
        ]
        []


percentString : Percentage -> String
percentString percentage =
    String.fromFloat percentage ++ "%"


cellSize : Cells -> Percentage
cellSize cells =
    100.0 / (Matrix.width cells |> toFloat)


innerCellSize : Cell -> Percentage
innerCellSize cell =
    case cell of
        Alive ->
            70

        Dead ->
            40


cellColorClass : Cell -> Coordinate -> Theme -> ClassName
cellColorClass cell { x, y } theme =
    case cell of
        Dead ->
            case theme of
                Light ->
                    "light-grey-cell"

                Dark ->
                    "dark-grey-cell"

        Alive ->
            case ( modBy 2 x == 0, modBy 2 y == 0 ) of
                ( True, True ) ->
                    "orange-cell"

                ( True, False ) ->
                    "green-cell"

                ( False, True ) ->
                    "blue-cell"

                ( False, False ) ->
                    "purple-cell"
