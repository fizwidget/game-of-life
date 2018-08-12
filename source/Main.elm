module Main exposing (main)

import Html
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (Html, toUnstyled, div, span, button, text)
import Html.Styled.Attributes exposing (css, class)
import Css exposing (..)
import Css.Colors as Colors
import Css.Transitions exposing (easeInOut, transition)
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
      , cells = Matrix.create { width = 18, height = 18 } Dead
      , previousCells = Nothing
      }
    , Cmd.none
    )



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
            else if isEquilibrium cells previousCells then
                { model | status = Paused }
            else
                model

        Paused ->
            model


isEquilibrium : Cells -> Maybe Cells -> Bool
isEquilibrium cells previousCells =
    previousCells
        |> Maybe.map (Matrix.equals cells)
        |> Maybe.withDefault False


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
                [ Css.Transitions.backgroundColor3 transitionDuration 0 easeInOut
                , Css.Transitions.width transitionDuration
                , Css.Transitions.height transitionDuration
                ]
            ]
        ]
        []


transitionDuration : Time
transitionDuration =
    550 * millisecond


cellContentSize : Cell -> Percentage
cellContentSize cell =
    case cell of
        Alive ->
            70

        Dead ->
            40


cellSize : Cells -> Percentage
cellSize cells =
    100.0 / toFloat (Matrix.height cells)


type alias Percentage =
    Float


cellColor : Cell -> Coordinate -> Css.Color
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
                button
                    [ onClick Pause, css (backgroundColor (rgba 179 186 197 0.6) :: statusButtonStyles) ]
                    [ text "Pause" ]

            Paused ->
                button
                    [ onClick Play, css (backgroundColor (rgba 54 179 126 0.9) :: statusButtonStyles) ]
                    [ text "Play" ]


statusButtonStyles : List Css.Style
statusButtonStyles =
    [ position fixed
    , width (px 100)
    , height (px 40)
    , marginLeft auto
    , marginRight auto
    , left (px 0)
    , right (px 0)
    , bottom (pct 6)
    , border2 (px 0) none
    , borderRadius (px 20)
    , color Colors.white
    , fontSize (px 20)
    , transition
        [ Css.Transitions.backgroundColor3 200 0 easeInOut
        , Css.Transitions.visibility3 200 0 easeInOut
        ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { status } =
    case status of
        Playing ->
            Time.every (600 * millisecond) (always Tick)

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
