module Main exposing (main)

import Html
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (Html, toUnstyled, div, button, text)
import Html.Styled.Attributes exposing (css)
import Css exposing (..)
import Css.Colors as Colors
import Css.Transitions as Transitions exposing (easeInOut, transition)
import Time as Time exposing (Time, millisecond)
import Matrix as Matrix exposing (Matrix, Coordinate)


-- MODEL


type Status
    = Paused
    | Playing


type Cell
    = Alive
    | Dead


type alias Cells =
    Matrix Cell


type alias Model =
    { status : Status
    , cells : Cells
    , previousCells : Maybe Cells
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { status = Paused
      , cells = lineConfiguration
      , previousCells = Nothing
      }
    , Cmd.none
    )


emptyConfiguration : Cells
emptyConfiguration =
    Matrix.create { width = 18, height = 18 } Dead


lineConfiguration : Cells
lineConfiguration =
    Matrix.create { width = 18, height = 18 } Dead
        |> Matrix.set { x = 5, y = 5 } Alive
        |> Matrix.set { x = 6, y = 5 } Alive
        |> Matrix.set { x = 7, y = 5 } Alive
        |> Matrix.set { x = 8, y = 5 } Alive
        |> Matrix.set { x = 9, y = 5 } Alive
        |> Matrix.set { x = 10, y = 5 } Alive
        |> Matrix.set { x = 11, y = 5 } Alive
        |> Matrix.set { x = 12, y = 5 } Alive



-- UPDATE


type Msg
    = Tick
    | Toggle Coordinate
    | Play
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            { model | status = Playing }
                |> noCmd

        Pause ->
            { model | status = Paused }
                |> noCmd

        Tick ->
            { model | cells = updateCells model.cells, previousCells = Just model.cells }
                |> pauseIfFinished
                |> noCmd

        Toggle coordinate ->
            { model | cells = toggleCoordinate coordinate model.cells }
                |> noCmd


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


updateCells : Cells -> Cells
updateCells cells =
    Matrix.coordinateMap (updateCell cells) cells


updateCell : Cells -> Coordinate -> Cell -> Cell
updateCell cells coordinate cell =
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


pauseIfFinished : Model -> Model
pauseIfFinished ({ status, cells, previousCells } as model) =
    case status of
        Playing ->
            if Matrix.all ((==) Dead) cells then
                { model | status = Paused }
            else if previousCells == Just cells then
                { model | status = Paused }
            else
                model

        Paused ->
            model


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



-- VIEW


view : Model -> Html Msg
view { cells, status } =
    div
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]
        [ squareContainer (viewCells cells)
        , viewStatusButton status cells
        ]


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


viewCells : Cells -> Html Msg
viewCells cells =
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
            |> Matrix.coordinateMap (viewCell (cellSize cells))
            |> Matrix.toList
        )


viewCell : Percentage -> Coordinate -> Cell -> Html Msg
viewCell size coordinate cell =
    div
        [ css
            [ width (pct size)
            , height (pct size)
            , displayFlex
            , justifyContent center
            , alignItems center
            ]
        , onClick (Toggle coordinate)
        ]
        [ viewCellContent cell coordinate ]


viewCellContent : Cell -> Coordinate -> Html msg
viewCellContent cell coordinate =
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


transitionDuration : Time
transitionDuration =
    tickInterval + 200 * millisecond


cellSize : Cells -> Percentage
cellSize cells =
    100.0 / toFloat (Matrix.height cells)


cellContentSize : Cell -> Percentage
cellContentSize cell =
    case cell of
        Alive ->
            70

        Dead ->
            40


type alias Percentage =
    Float


cellColor : Cell -> Coordinate -> Color
cellColor cell { x, y } =
    case cell of
        Alive ->
            case ( x % 2 == 0, y % 2 == 0 ) of
                ( True, True ) ->
                    rgba 255 171 0 0.8

                ( True, False ) ->
                    rgba 54 179 126 0.8

                ( False, True ) ->
                    rgba 0 184 217 0.8

                ( False, False ) ->
                    rgba 101 84 192 0.8

        Dead ->
            rgb 244 245 247


viewStatusButton : Status -> Cells -> Html Msg
viewStatusButton status cells =
    if Matrix.all ((==) Dead) cells then
        div [] []
    else
        case status of
            Playing ->
                viewButton "Pause" Pause (backgroundColor (rgba 179 186 197 0.6) :: statusButtonStyles)

            Paused ->
                viewButton "Play" Play (backgroundColor (rgba 54 179 126 0.9) :: statusButtonStyles)


statusButtonStyles : List Style
statusButtonStyles =
    [ position fixed
    , width (px 100)
    , height (px 40)
    , left (px 20)
    , bottom (px 20)
    , border2 (px 0) none
    , borderRadius (px 15)
    , color Colors.white
    , fontSize (px 20)
    , transition
        [ Transitions.backgroundColor3 200 0 easeInOut
        , Transitions.visibility3 200 0 easeInOut
        ]
    ]


viewButton : String -> Msg -> List Style -> Html Msg
viewButton description clickMsg styles =
    button
        [ onClick clickMsg, css styles ]
        [ text description ]



-- SUBSCRIPTIONS


tickInterval : Time
tickInterval =
    600 * millisecond


subscriptions : Model -> Sub Msg
subscriptions { status } =
    case status of
        Playing ->
            Time.every tickInterval (always Tick)

        Paused ->
            Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
