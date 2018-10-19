module GameOfLife exposing
    ( Cell(..)
    , Events
    , GameOfLife
    , Padding(..)
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


beginWithPattern : Dimensions -> Padding -> Pattern -> GameOfLife
beginWithPattern minDimensions padding pattern =
    let
        paddingCells =
            case padding of
                WithPadding ->
                    6

                WithoutPadding ->
                    0

        width =
            max (Pattern.width pattern + paddingCells) minDimensions.width

        height =
            max (Pattern.height pattern + paddingCells) minDimensions.height

        center =
            { x = width // 2
            , y = height // 2
            }

        centeredPattern =
            Pattern.centerAt center pattern

        deadCells =
            Matrix.create { width = width, height = height } Dead
    in
    GameOfLife (bringPatternToLife deadCells centeredPattern)


bringPatternToLife : Cells -> Pattern -> Cells
bringPatternToLife cells pattern =
    let
        makeAlive =
            Matrix.set Alive

        patternCoordinates =
            Pattern.toCoordinates pattern
    in
    List.foldl makeAlive cells patternCoordinates



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


type Percentage
    = Percentage Float


type alias ClassName =
    String


type Zoom
    = Far
    | Normal
    | Close


type Theme
    = Light
    | Dark


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

        size =
            outerCellSize cells
    in
    div
        attributes
        (cells
            |> Matrix.coordinateMap (viewCell size theme events)
            |> Matrix.toList
        )


zoomStyles : Zoom -> List (Attribute msg)
zoomStyles zoom =
    let
        percentage =
            case zoom of
                Far ->
                    Percentage 100

                Normal ->
                    Percentage 150

                Close ->
                    Percentage 200
    in
    [ style "width" (percentString percentage)
    , style "height" (percentString percentage)
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
percentString (Percentage percentage) =
    String.fromFloat percentage ++ "%"


outerCellSize : Cells -> Percentage
outerCellSize cells =
    Matrix.width cells
        |> toFloat
        |> (\width -> 100 / width)
        |> Percentage


innerCellSize : Cell -> Percentage
innerCellSize cell =
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
