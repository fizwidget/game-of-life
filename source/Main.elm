module Main exposing (main)

import Html
import Html.Styled.Events exposing (onClick)
import Html.Styled exposing (Html, toUnstyled, div, span, button, text)
import Html.Styled.Attributes exposing (css, class)
import Css exposing (..)
import Css.Colors as Colors
import Css.Transitions exposing (easeInOut, transition)
import Time as Time exposing (millisecond)
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
            { model | cells = toggleCoordinate model.cells coordinate }
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
    Matrix.neighbours coordinate cells
        |> List.filter ((==) Alive)
        |> List.length


pauseIfFinished : Model -> Model
pauseIfFinished ({ status, cells, previousCells } as model) =
    case status of
        Playing ->
            if Matrix.all ((==) Dead) cells then
                { model | status = Paused }
            else if hasReachedEquilibrium cells previousCells then
                { model | status = Paused }
            else
                model

        Paused ->
            model


hasReachedEquilibrium : Cells -> Maybe Cells -> Bool
hasReachedEquilibrium cells previousCells =
    previousCells
        |> Maybe.map (Matrix.equals cells)
        |> Maybe.withDefault False


toggleCoordinate : Cells -> Coordinate -> Cells
toggleCoordinate cells coordinate =
    Matrix.update coordinate cells toggleCell


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
    div [ css [ displayFlex, justifyContent center, alignItems center ] ]
        [ viewCells cells, viewStatusButton status cells ]


viewCells : Cells -> Html Msg
viewCells cells =
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
        [ div
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
            (Matrix.toList cells |> List.map (viewCell (cellSize cells)))
        ]


viewCell : Float -> ( Coordinate, Cell ) -> Html Msg
viewCell size ( coordinate, cell ) =
    div
        [ css
            [ height (pct size)
            , backgroundColor (cellColor cell coordinate)
            , displayFlex
            , flex3 (int 0) (int 0) (pct size)
            , borderRadius (pct 50)
            , border3 (px (cellBorderSize cell)) solid Colors.white
            , boxSizing borderBox
            , transition
                [ Css.Transitions.backgroundColor3 200 0 easeInOut
                , Css.Transitions.borderWidth 200
                ]
            ]
        , (onClick (Toggle coordinate))
        ]
        []


cellBorderSize : Cell -> Float
cellBorderSize cell =
    case cell of
        Alive ->
            4

        Dead ->
            30


cellSize : Cells -> Float
cellSize cells =
    100.0 / toFloat (Matrix.height cells)


cellColor : Cell -> Coordinate -> Css.Color
cellColor cell coordinate =
    case cell of
        Alive ->
            liveCellColor coordinate

        Dead ->
            rgb 244 245 247


liveCellColor : Coordinate -> Color
liveCellColor { x, y } =
    case ( x % 2 == 0, y % 2 == 0 ) of
        ( True, True ) ->
            rgba 255 171 0 0.8

        ( True, False ) ->
            rgba 54 179 126 0.8

        ( False, True ) ->
            rgba 0 184 217 0.8

        ( False, False ) ->
            rgba 101 84 192 0.8


viewStatusButton : Status -> Cells -> Html Msg
viewStatusButton status cells =
    let
        styles =
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
    in
        if Matrix.all ((==) Dead) cells then
            div [] []
        else
            case status of
                Playing ->
                    button
                        [ onClick Pause, css (backgroundColor (rgba 179 186 197 0.6) :: styles) ]
                        [ text "Pause" ]

                Paused ->
                    button
                        [ onClick Play, css (backgroundColor (rgba 54 179 126 0.9) :: styles) ]
                        [ text "Play" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { status } =
    case status of
        Playing ->
            Time.every (millisecond * 200) (always Tick)

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
