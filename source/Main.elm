module Main exposing (main)

import Html
import Html.Styled.Events exposing (onClick, onMouseDown, onMouseUp, onMouseEnter)
import Html.Styled exposing (Html, toUnstyled, div, button, text)
import Html.Styled.Attributes exposing (css)
import Css exposing (..)
import Css.Colors as Colors
import Css.Transitions as Transitions exposing (easeInOut, transition)
import Keyboard as Keyboard exposing (KeyCode)
import Char
import Time as Time exposing (Time, millisecond)
import Matrix as Matrix exposing (Matrix, Coordinate)
import History as History exposing (History)


-- MODEL


type Status
    = Paused
    | Playing


type Mouse
    = Up
    | Down


type Speed
    = Slow
    | Fast


type Cell
    = Alive
    | Dead


type alias Cells =
    Matrix Cell


type alias Model =
    { status : Status
    , cells : History Cells
    , mouse : Mouse
    , speed : Speed
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { status = Paused
      , cells = History.begin initialCells
      , mouse = Up
      , speed = Slow
      }
    , Cmd.none
    )


initialCells : Cells
initialCells =
    Matrix.create { width = 18, height = 18 } Dead



-- UPDATE


type Msg
    = Play
    | Pause
    | Tick
    | SetSpeed Speed
    | MouseDown Coordinate
    | MouseUp
    | MouseOver Coordinate
    | KeyDown KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ status, mouse, cells } as model) =
    case msg of
        Play ->
            { model | status = Playing }
                |> noCmd

        Pause ->
            { model | status = Paused }
                |> noCmd

        Tick ->
            { model | cells = History.record updateCells cells }
                |> pauseWhenSettled
                |> noCmd

        SetSpeed speed ->
            { model | speed = speed }
                |> noCmd

        MouseDown coordinate ->
            { model | mouse = Down, cells = History.record (toggleCoordinate coordinate) cells }
                |> noCmd

        MouseUp ->
            { model | mouse = Up }
                |> noCmd

        MouseOver coordinate ->
            case mouse of
                Up ->
                    noCmd model

                Down ->
                    { model | cells = History.record (toggleCoordinate coordinate) cells }
                        |> noCmd

        KeyDown keyCode ->
            if keyCode == Char.toCode 'P' then
                { model | status = toggleStatus status }
                    |> noCmd
            else if keyCode == rightKey then
                { model | status = Paused, cells = History.record updateCells cells }
                    |> noCmd
            else if keyCode == leftKey then
                { model | status = Paused, cells = History.undo cells }
                    |> noCmd
            else
                noCmd model


leftKey : KeyCode
leftKey =
    37


rightKey : KeyCode
rightKey =
    39


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


toggleStatus : Status -> Status
toggleStatus status =
    case status of
        Playing ->
            Paused

        Paused ->
            Playing


updateCells : Cells -> Cells
updateCells cells =
    Matrix.coordinateMap (updateCell cells) cells


updateCell : Cells -> Coordinate -> Cell -> Cell
updateCell cells coordinate cell =
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


pauseWhenSettled : Model -> Model
pauseWhenSettled ({ status, cells } as model) =
    case status of
        Playing ->
            if History.didChange cells then
                model
            else
                { model | status = Paused }

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
view { cells, status, speed } =
    let
        currentCells =
            History.now cells

        transitionDuration =
            getTransitionDuration speed
    in
        div
            [ css
                [ displayFlex
                , justifyContent center
                , alignItems center
                ]
            ]
            [ squareContainer (viewCells transitionDuration currentCells)
            , viewControls status speed currentCells
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


viewCells : Time -> Cells -> Html Msg
viewCells transitionDuration cells =
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
            |> Matrix.coordinateMap (viewCell transitionDuration (cellSize cells))
            |> Matrix.toList
        )


viewCell : Time -> Percentage -> Coordinate -> Cell -> Html Msg
viewCell transitionDuration size coordinate cell =
    div
        [ css
            [ width (pct size)
            , height (pct size)
            , displayFlex
            , justifyContent center
            , alignItems center
            ]
        , onMouseDown (MouseDown coordinate)
        , onMouseUp MouseUp
        , onMouseEnter (MouseOver coordinate)
        ]
        [ viewCellContent transitionDuration cell coordinate ]


viewCellContent : Time -> Cell -> Coordinate -> Html msg
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


getTransitionDuration : Speed -> Time
getTransitionDuration speed =
    (tickInterval speed) + 200 * millisecond


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


viewControls : Status -> Speed -> Cells -> Html Msg
viewControls status speed cells =
    div
        [ css
            [ position fixed
            , left (px 20)
            , bottom (px 20)
            ]
        ]
        [ viewStatusButton status cells
        , viewSpeedButton status speed
        ]


viewStatusButton : Status -> Cells -> Html Msg
viewStatusButton status cells =
    if Matrix.all ((==) Dead) cells then
        blank
    else
        case status of
            Playing ->
                viewButton "Pause" Pause []

            Paused ->
                viewButton "Play" Play [ backgroundColor (rgba 54 179 126 0.9) ]


viewSpeedButton : Status -> Speed -> Html Msg
viewSpeedButton status speed =
    case ( status, speed ) of
        ( Playing, Slow ) ->
            viewButton "Faster" (SetSpeed Fast) []

        ( Playing, Fast ) ->
            viewButton "Slower" (SetSpeed Slow) []

        _ ->
            blank


viewButton : String -> Msg -> List Style -> Html Msg
viewButton description clickMsg styles =
    button
        [ onClick clickMsg, css (buttonStyles ++ styles) ]
        [ text description ]


buttonStyles : List Style
buttonStyles =
    [ width (px 100)
    , height (px 40)
    , margin (px 5)
    , border2 (px 0) none
    , borderRadius (px 15)
    , color Colors.white
    , backgroundColor (rgba 179 186 197 0.6)
    , fontSize (px 20)
    , transition
        [ Transitions.backgroundColor3 200 0 easeInOut
        , Transitions.visibility3 200 0 easeInOut
        ]
    ]


blank : Html msg
blank =
    div [] []



-- SUBSCRIPTIONS


tickInterval : Speed -> Time
tickInterval speed =
    case speed of
        Slow ->
            600 * millisecond

        Fast ->
            300 * millisecond


subscriptions : Model -> Sub Msg
subscriptions { status, speed } =
    let
        ticks =
            case status of
                Playing ->
                    Time.every (tickInterval speed) (always Tick)

                Paused ->
                    Sub.none

        keyDowns =
            Keyboard.downs KeyDown
    in
        Sub.batch [ ticks, keyDowns ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
