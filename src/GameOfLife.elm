module GameOfLife exposing
    ( Cell(..)
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
import Msg exposing (Msg(..))
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
    GameOfLife (Matrix.create dimensions Dead)


beginWithPattern : Size -> Padding -> Pattern -> GameOfLife
beginWithPattern minimumSize padding pattern =
    let
        (Size size) =
            calculateSize minimumSize padding pattern

        center =
            { x = size // 2
            , y = size // 2
            }

        centeredPattern =
            Pattern.centerAt center pattern

        dimensions =
            { width = size
            , height = size
            }

        deadCells =
            Matrix.create dimensions Dead
    in
    GameOfLife (bringPatternToLife deadCells centeredPattern)


calculateSize : Size -> Padding -> Pattern -> Size
calculateSize (Size minimumSize) padding pattern =
    Pattern.size pattern
        |> (+) (paddingAmount padding pattern)
        |> max minimumSize
        |> Size


paddingAmount : Padding -> Pattern -> Int
paddingAmount padding pattern =
    case padding of
        WithPadding ->
            Pattern.size pattern // 4

        WithoutPadding ->
            0


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


type alias Percentage =
    Float


type alias ClassName =
    String


view : GameOfLife -> Zoom -> Theme -> Html Msg
view game zoom theme =
    div
        [ class "square-container" ]
        [ viewGame game zoom theme ]


viewGame : GameOfLife -> Zoom -> Theme -> Html Msg
viewGame (GameOfLife cells) zoom theme =
    let
        zoomPercentage =
            calculateZoomPercentage zoom

        coordinateSize =
            calculateCoordinateSize cells
    in
    div
        [ class "game-container"
        , style "width" (percentageStyle zoomPercentage)
        , style "height" (percentageStyle zoomPercentage)
        ]
        (cells
            |> Matrix.coordinateMap (viewCoordinate coordinateSize theme)
            |> Matrix.toList
        )


viewCoordinate : Percentage -> Theme -> Coordinate -> Cell -> Html Msg
viewCoordinate relativeSize theme coordinate cell =
    div
        [ class "coordinate"
        , style "width" (percentageStyle relativeSize)
        , style "height" (percentageStyle relativeSize)
        , onMouseDown (MouseDown coordinate)
        , onMouseUp MouseUp
        , onMouseEnter (MouseOver coordinate)
        ]
        [ viewCell cell coordinate theme ]


viewCell : Cell -> Coordinate -> Theme -> Html Msg
viewCell cell coordinate theme =
    div
        [ class "cell"
        , class (cellStatusClass cell)
        , class (cellColorClass cell coordinate theme)
        ]
        []


calculateCoordinateSize : Cells -> Percentage
calculateCoordinateSize cells =
    Matrix.width cells
        |> toFloat
        |> (\width -> 100 / width)


calculateZoomPercentage : Zoom -> Percentage
calculateZoomPercentage zoom =
    case zoom of
        Far ->
            100

        Normal ->
            150

        Close ->
            200


cellStatusClass : Cell -> ClassName
cellStatusClass cell =
    case cell of
        Alive ->
            "alive"

        Dead ->
            "dead"


cellColorClass : Cell -> Coordinate -> Theme -> ClassName
cellColorClass cell { x, y } theme =
    case cell of
        Dead ->
            case theme of
                Light ->
                    "light-grey"

                Dark ->
                    "dark-grey"

        Alive ->
            case ( modBy 2 x == 0, modBy 2 y == 0 ) of
                ( True, True ) ->
                    "orange"

                ( True, False ) ->
                    "green"

                ( False, True ) ->
                    "blue"

                ( False, False ) ->
                    "purple"


percentageStyle : Percentage -> String
percentageStyle percentage =
    String.fromFloat percentage ++ "%"
