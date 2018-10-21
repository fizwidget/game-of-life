module GameOfLife exposing
    ( Cell(..)
    , Events
    , GameOfLife
    , Padding(..)
    , Size(..)
    , Theme(..)
    , Zoom(..)
    , begin
    , beginWithPattern
    , isFinished
    , step
    , toggleCell
    , view
    )

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onMouseDown, onMouseEnter, onMouseUp)
import Matrix exposing (Coordinate, Dimensions, Matrix)
import Pattern exposing (Pattern)



-- TYPES


type Cell
    = Alive
    | Dead


type alias Cells =
    Matrix Cell


type GameOfLife
    = GameOfLife Cells


type Size
    = Size Int


type Padding
    = WithPadding
    | WithoutPadding



-- CREATION


begin : Size -> GameOfLife
begin (Size size) =
    let
        dimensions =
            { width = size
            , height = size
            }
    in
    Matrix.create dimensions Dead
        |> GameOfLife


beginWithPattern : Size -> Padding -> Pattern -> GameOfLife
beginWithPattern minimumSize padding pattern =
    let
        (Size actualSize) =
            calculateSize pattern padding minimumSize

        center =
            { x = actualSize // 2
            , y = actualSize // 2
            }

        centeredPattern =
            Pattern.centerAt center pattern

        dimensions =
            { width = actualSize
            , height = actualSize
            }

        deadCells =
            Matrix.create dimensions Dead
    in
    GameOfLife (bringPatternToLife deadCells centeredPattern)


calculateSize : Pattern -> Padding -> Size -> Size
calculateSize pattern padding (Size minimumSize) =
    let
        paddingAmount =
            case padding of
                WithPadding ->
                    6

                WithoutPadding ->
                    0
    in
    max (Pattern.width pattern) (Pattern.height pattern)
        |> (+) paddingAmount
        |> max minimumSize
        |> Size


bringPatternToLife : Cells -> Pattern -> Cells
bringPatternToLife cells pattern =
    let
        makeAlive =
            Matrix.set Alive

        coordinates =
            Pattern.toCoordinates pattern
    in
    List.foldl makeAlive cells coordinates



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
    Matrix.update toggleCellHelper coordinate cells
        |> GameOfLife


toggleCellHelper : Cell -> Cell
toggleCellHelper cell =
    case cell of
        Alive ->
            Dead

        Dead ->
            Alive


isFinished : GameOfLife -> Bool
isFinished (GameOfLife cells) =
    Matrix.all ((==) Dead) cells



-- VIEW


type Zoom
    = Far
    | Normal
    | Close


type Theme
    = Light
    | Dark


type Percentage
    = Percentage Float


type ClassName
    = ClassName String


type alias Events msg =
    { onMouseOver : Coordinate -> msg
    , onMouseDown : Coordinate -> msg
    , onMouseUp : msg
    }


view : GameOfLife -> Zoom -> Theme -> Events msg -> Html msg
view game zoom theme events =
    div
        [ class "square-container" ]
        [ viewGame game zoom theme events ]


viewGame : GameOfLife -> Zoom -> Theme -> Events msg -> Html msg
viewGame (GameOfLife cells) zoom theme events =
    let
        gameSize =
            calculateGameSize zoom

        coordinateSize =
            calculateCoordinateSize cells
    in
    div
        [ class "cells"
        , style "width" (percentStyleValue gameSize)
        , style "height" (percentStyleValue gameSize)
        ]
        (cells
            |> Matrix.coordinateMap (viewCoordinate coordinateSize theme events)
            |> Matrix.toList
        )


viewCoordinate : Percentage -> Theme -> Events msg -> Coordinate -> Cell -> Html msg
viewCoordinate relativeSize theme events coordinate cell =
    div
        [ class "coordinate"
        , style "width" (percentStyleValue relativeSize)
        , style "height" (percentStyleValue relativeSize)
        , onMouseDown (events.onMouseDown coordinate)
        , onMouseUp events.onMouseUp
        , onMouseEnter (events.onMouseOver coordinate)
        ]
        [ viewCell cell coordinate theme ]


viewCell : Cell -> Coordinate -> Theme -> Html msg
viewCell cell coordinate theme =
    let
        size =
            calculateCellSize cell

        (ClassName colorClass) =
            cellColorClass cell coordinate theme
    in
    div
        [ class "cell"
        , class colorClass
        , style "width" (percentStyleValue size)
        , style "height" (percentStyleValue size)
        ]
        []


percentStyleValue : Percentage -> String
percentStyleValue (Percentage percentage) =
    String.fromFloat percentage ++ "%"


calculateCoordinateSize : Cells -> Percentage
calculateCoordinateSize cells =
    Matrix.width cells
        |> toFloat
        |> (\width -> 100 / width)
        |> Percentage


calculateGameSize : Zoom -> Percentage
calculateGameSize zoom =
    case zoom of
        Far ->
            Percentage 100

        Normal ->
            Percentage 150

        Close ->
            Percentage 200


calculateCellSize : Cell -> Percentage
calculateCellSize cell =
    case cell of
        Alive ->
            Percentage 70

        Dead ->
            Percentage 40


cellColorClass : Cell -> Coordinate -> Theme -> ClassName
cellColorClass cell { x, y } theme =
    case cell of
        Dead ->
            case theme of
                Light ->
                    ClassName "light-grey-cell"

                Dark ->
                    ClassName "dark-grey-cell"

        Alive ->
            case ( modBy 2 x == 0, modBy 2 y == 0 ) of
                ( True, True ) ->
                    ClassName "orange-cell"

                ( True, False ) ->
                    ClassName "green-cell"

                ( False, True ) ->
                    ClassName "blue-cell"

                ( False, False ) ->
                    ClassName "purple-cell"
