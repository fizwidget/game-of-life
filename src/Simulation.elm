module Simulation exposing
    ( Cell(..)
    , Cells
    , begin
    , beginWithPattern
    , isFinished
    , step
    , toggleCoordinate
    , view
    )

import Css exposing (..)
import Css.Transitions as Transitions exposing (easeInOut, transition)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onMouseDown, onMouseEnter, onMouseUp)
import Matrix exposing (Coordinate, Matrix)
import Pattern exposing (Pattern)



-- TYPES


type Cell
    = Alive
    | Dead


type alias Cells =
    Matrix Cell



-- CREATE


begin : Cells
begin =
    Matrix.create { width = 18, height = 18 } Dead


beginWithPattern : Pattern -> Cells
beginWithPattern pattern =
    let
        width =
            Pattern.width pattern
                |> (*) 2
                |> max 18

        height =
            Pattern.height pattern
                |> (*) 2
                |> max 18

        size =
            { width = width
            , height = height
            }

        center =
            { x = width // 2
            , y = height // 2
            }

        centeredPattern =
            Pattern.centerAt center pattern

        emptyMatrix =
            Matrix.create size Dead
    in
    List.foldl
        (Matrix.set Alive)
        emptyMatrix
        (Pattern.toCoordinates centeredPattern)



-- SIMULATION


type alias Milliseconds =
    Float



-- STEP


step : Cells -> Cells
step cells =
    Matrix.coordinateMap (stepCell cells) cells


stepCell : Cells -> Coordinate -> Cell -> Cell
stepCell cells coordinate cell =
    case ( cell, liveNeighbours cells coordinate ) of
        ( Alive, 2 ) ->
            Alive

        ( Alive, 3 ) ->
            Alive

        ( Dead, 3 ) ->
            Alive

        _ ->
            Dead


liveNeighbours : Cells -> Coordinate -> Int
liveNeighbours cells coordinate =
    Matrix.neighbours cells coordinate
        |> List.filter ((==) Alive)
        |> List.length



-- TOGGLE


toggleCoordinate : Coordinate -> Cells -> Cells
toggleCoordinate coordinate cells =
    Matrix.update toggleCell coordinate cells


toggleCell : Cell -> Cell
toggleCell cell =
    case cell of
        Alive ->
            Dead

        Dead ->
            Alive



-- UTILS


isFinished : Cells -> Bool
isFinished =
    Matrix.all ((==) Dead)



-- VIEW


type alias Handlers msg =
    { mouseOver : Coordinate -> msg
    , mouseDown : Coordinate -> msg
    , mouseUp : msg
    }


type alias Percentage =
    Float


view : Milliseconds -> Cells -> Handlers msg -> Html msg
view transitionDuration cells handlers =
    squareContainer <|
        div
            [ css
                [ displayFlex
                , alignItems center
                , justifyContent center
                , flexWrap wrap
                , position absolute
                , width (pct 100)
                , height (pct 100)
                ]
            ]
            (cells
                |> Matrix.coordinateMap (viewCell transitionDuration (cellSize cells) handlers)
                |> Matrix.toList
            )


viewCell : Milliseconds -> Percentage -> Handlers msg -> Coordinate -> Cell -> Html msg
viewCell transitionDuration size handlers coordinate cell =
    div
        [ css
            [ width (pct size)
            , height (pct size)
            , displayFlex
            , justifyContent center
            , alignItems center
            ]
        , onMouseDown (handlers.mouseDown coordinate)
        , onMouseUp handlers.mouseUp
        , onMouseEnter (handlers.mouseOver coordinate)
        ]
        [ viewCellContent transitionDuration cell coordinate ]


viewCellContent : Milliseconds -> Cell -> Coordinate -> Html msg
viewCellContent transitionDuration cell coordinate =
    div
        [ css
            [ width (pct (cellContentSize cell))
            , height (pct (cellContentSize cell))
            , backgroundColor (cellColor cell coordinate)
            , borderRadius (pct 30)
            , transition
                [ Transitions.backgroundColor3 transitionDuration 0 easeInOut
                , Transitions.width transitionDuration
                , Transitions.height transitionDuration
                ]
            ]
        ]
        []


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


cellColor : Cell -> Coordinate -> Color
cellColor cell { x, y } =
    case cell of
        Dead ->
            rgb 244 245 247

        Alive ->
            case ( modBy 2 x == 0, modBy 2 y == 0 ) of
                ( True, True ) ->
                    rgba 255 171 0 0.8

                ( True, False ) ->
                    rgba 54 179 126 0.8

                ( False, True ) ->
                    rgba 0 184 217 0.8

                ( False, False ) ->
                    rgba 101 84 192 0.8


squareContainer : Html msg -> Html msg
squareContainer content =
    div
        [ css
            [ position relative
            , width (pct 100)
            , after
                [ property "content" "''"
                , display block
                , paddingBottom (pct 100)
                ]
            ]
        ]
        [ content ]
