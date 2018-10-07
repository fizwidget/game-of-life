module Simulation exposing
    ( Cell(..)
    , Simulation
    , begin
    , beginWithPattern
    , isFinished
    , step
    , toggleCell
    , view
    )

import Html exposing (Html, div)
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


type Simulation
    = Simulation Cells



-- CREATE


begin : Simulation
begin =
    Matrix.create { width = 18, height = 18 } Dead
        |> Simulation


beginWithPattern : Pattern -> Simulation
beginWithPattern pattern =
    let
        size =
            max (Pattern.width pattern) (Pattern.height pattern)
                |> toFloat
                |> (*) 1.2
                |> max 18
                |> Basics.round

        center =
            { x = size // 2
            , y = size // 2
            }

        centeredPattern =
            Pattern.centerAt center pattern

        deadCells =
            Matrix.create { width = size, height = size } Dead
    in
    Simulation <|
        List.foldl
            (Matrix.set Alive)
            deadCells
            (Pattern.toCoordinates centeredPattern)



-- OPERATIONS


step : Simulation -> Simulation
step (Simulation cells) =
    Matrix.coordinateMap (stepCell cells) cells
        |> Simulation


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


toggleCell : Coordinate -> Simulation -> Simulation
toggleCell coordinate (Simulation cells) =
    let
        toggle cell =
            case cell of
                Alive ->
                    Dead

                Dead ->
                    Alive
    in
    Matrix.update toggle coordinate cells
        |> Simulation


isFinished : Simulation -> Bool
isFinished (Simulation cells) =
    Matrix.all ((==) Dead) cells



-- VIEW


type alias Percentage =
    Float


type alias Milliseconds =
    Float


type alias Handlers msg =
    { mouseOver : Coordinate -> msg
    , mouseDown : Coordinate -> msg
    , mouseUp : msg
    }


view : Milliseconds -> Simulation -> Handlers msg -> Html msg
view transitionDuration simulation handlers =
    squareContainer <|
        div
            [ class "cells" ]
            (viewSimulation transitionDuration simulation handlers)


viewSimulation : Milliseconds -> Simulation -> Handlers msg -> List (Html msg)
viewSimulation transitionDuration (Simulation cells) handlers =
    cells
        |> Matrix.coordinateMap (viewCell transitionDuration (cellSize cells) handlers)
        |> Matrix.toList


viewCell : Milliseconds -> Percentage -> Handlers msg -> Coordinate -> Cell -> Html msg
viewCell transitionDuration size handlers coordinate cell =
    div
        [ class "center-content"
        , style "width" (percentage size)
        , style "height" (percentage size)
        , onMouseDown (handlers.mouseDown coordinate)
        , onMouseUp handlers.mouseUp
        , onMouseEnter (handlers.mouseOver coordinate)
        ]
        [ viewCellContent transitionDuration cell coordinate ]


viewCellContent : Milliseconds -> Cell -> Coordinate -> Html msg
viewCellContent transitionDuration cell coordinate =
    let
        size =
            cellContentSize cell
    in
    div
        [ class "cell-content"
        , class (cellColor cell coordinate)
        , style "width" (percentage size)
        , style "height" (percentage size)
        ]
        []


percentage : Percentage -> String
percentage value =
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


cellColor : Cell -> Coordinate -> String
cellColor cell { x, y } =
    case cell of
        Dead ->
            "dead-cell"

        Alive ->
            case ( modBy 2 x == 0, modBy 2 y == 0 ) of
                ( True, True ) ->
                    "live-cell-1"

                ( True, False ) ->
                    "live-cell-2"

                ( False, True ) ->
                    "live-cell-3"

                ( False, False ) ->
                    "live-cell-4"


squareContainer : Html msg -> Html msg
squareContainer content =
    div
        [ class "square-container" ]
        [ content ]
