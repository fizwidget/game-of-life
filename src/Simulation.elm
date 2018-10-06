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
            (viewSimulation transitionDuration simulation handlers)


viewSimulation : Milliseconds -> Simulation -> Handlers msg -> List (Html msg)
viewSimulation transitionDuration (Simulation cells) handlers =
    cells
        |> Matrix.coordinateMap (viewCell transitionDuration (cellSize cells) handlers)
        |> Matrix.toList


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
