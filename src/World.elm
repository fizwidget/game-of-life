module World exposing
    ( Cell(..)
    , World
    , Zoom(..)
    , create
    , createWithPattern
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


type World
    = World Cells



-- CREATE


create : World
create =
    Matrix.create { width = 18, height = 18 } Dead
        |> World


createWithPattern : Pattern -> World
createWithPattern pattern =
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
    World <|
        List.foldl
            (Matrix.set Alive)
            deadCells
            (Pattern.toCoordinates centeredPattern)



-- OPERATIONS


step : World -> World
step (World cells) =
    Matrix.coordinateMap (stepCell cells) cells
        |> World


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


toggleCell : Coordinate -> World -> World
toggleCell coordinate (World cells) =
    let
        toggle cell =
            case cell of
                Alive ->
                    Dead

                Dead ->
                    Alive
    in
    Matrix.update toggle coordinate cells
        |> World


isFinished : World -> Bool
isFinished (World cells) =
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


type Zoom
    = Small
    | Medium
    | Large


view : Milliseconds -> World -> Zoom -> Handlers msg -> Html msg
view transitionDuration world zoom handlers =
    div
        [ class "square-container" ]
        [ viewCells transitionDuration world zoom handlers ]


viewCells : Milliseconds -> World -> Zoom -> Handlers msg -> Html msg
viewCells transitionDuration (World cells) zoom handlers =
    div
        ([ class "cells-container" ] ++ zoomStyles zoom)
        (cells
            |> Matrix.coordinateMap (viewCell transitionDuration (cellSize cells) handlers)
            |> Matrix.toList
        )


zoomStyles : Zoom -> List (Attribute msg)
zoomStyles zoom =
    let
        percentage =
            case zoom of
                Small ->
                    percentageStyle 100

                Medium ->
                    percentageStyle 150

                Large ->
                    percentageStyle 200
    in
    [ style "width" percentage
    , style "height" percentage
    ]


viewCell : Milliseconds -> Percentage -> Handlers msg -> Coordinate -> Cell -> Html msg
viewCell transitionDuration relativeSize handlers coordinate cell =
    div
        [ class "center-content"
        , style "width" (percentageStyle relativeSize)
        , style "height" (percentageStyle relativeSize)
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
