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
    | Undo
    | Redo
    | SetSpeed Speed
    | MouseDown Coordinate
    | MouseUp
    | MouseOver Coordinate
    | KeyDown KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg ({ status, mouse, cells } as model) =
    case msg of
        Play ->
            { model | status = Playing }

        Pause ->
            { model | status = Paused }

        Tick ->
            { model | cells = History.record step cells }
                |> pauseIfSettled

        Undo ->
            undo model

        Redo ->
            redo model

        SetSpeed speed ->
            { model | speed = speed }

        MouseDown coordinate ->
            { model
                | mouse = Down
                , cells = History.record (toggleCoordinate coordinate) cells
            }

        MouseUp ->
            { model | mouse = Up }

        MouseOver coordinate ->
            case mouse of
                Up ->
                    model

                Down ->
                    { model | cells = History.record (toggleCoordinate coordinate) cells }

        KeyDown keyCode ->
            if keyCode == leftArrow then
                undo model
            else if keyCode == rightArrow then
                redo model
            else if keyCode == Char.toCode 'P' then
                toggleStatus model
            else
                model


undo : Model -> Model
undo model =
    { model
        | status = Paused
        , cells = History.undo model.cells
    }


redo : Model -> Model
redo ({ cells } as model) =
    { model
        | status = Paused
        , cells = History.redo cells |> Maybe.withDefault (History.record step cells)
    }


toggleStatus : Model -> Model
toggleStatus model =
    case model.status of
        Playing ->
            { model | status = Paused }

        Paused ->
            { model | status = Playing }


pauseIfSettled : Model -> Model
pauseIfSettled ({ status, cells } as model) =
    if History.didChange cells then
        model
    else
        { model | status = Paused }


step : Cells -> Cells
step cells =
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
    let
        cellHeight =
            toFloat (Matrix.height cells)
    in
        100.0 / cellHeight


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
        Dead ->
            rgb 244 245 247

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


viewControls : Status -> Speed -> Cells -> Html Msg
viewControls status speed cells =
    div []
        [ bottomLeft
            [ viewStatusButton status |> ifNotBlank cells
            , viewSpeedButton status speed
            ]
        , bottomRight
            [ viewUndoButton status
            , viewRedoButton status
            ]
            |> ifNotBlank cells
        ]


bottomLeft : List (Html msg) -> Html msg
bottomLeft =
    div
        [ css
            [ position fixed
            , left (px 20)
            , bottom (px 20)
            ]
        ]


bottomRight : List (Html msg) -> Html msg
bottomRight =
    div
        [ css
            [ position fixed
            , right (px 20)
            , bottom (px 20)
            ]
        ]


ifNotBlank : Cells -> Html msg -> Html msg
ifNotBlank cells children =
    if Matrix.all ((==) Dead) cells then
        div [] []
    else
        children


viewStatusButton : Status -> Html Msg
viewStatusButton status =
    case status of
        Playing ->
            viewButton "Pause" Pause []

        Paused ->
            viewButton "Play" Play [ backgroundColor (rgba 54 179 126 0.8) ]


viewSpeedButton : Status -> Speed -> Html Msg
viewSpeedButton status speed =
    case ( status, speed ) of
        ( Playing, Slow ) ->
            viewButton "Faster" (SetSpeed Fast) []

        ( Playing, Fast ) ->
            viewButton "Slower" (SetSpeed Slow) []

        ( Paused, _ ) ->
            div [] []


viewUndoButton : Status -> Html Msg
viewUndoButton status =
    viewButton "⬅︎" Undo (undoRedoButtonStyles status)


viewRedoButton : Status -> Html Msg
viewRedoButton status =
    viewButton "➡︎" Redo (undoRedoButtonStyles status)


undoRedoButtonStyles : Status -> List Style
undoRedoButtonStyles status =
    case status of
        Playing ->
            []

        Paused ->
            [ backgroundColor <| rgba 80 95 121 0.6 ]


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { status, speed } =
    Sub.batch
        [ Keyboard.downs KeyDown
        , tickSubscription status speed
        ]


tickSubscription : Status -> Speed -> Sub Msg
tickSubscription status speed =
    case status of
        Playing ->
            Time.every (tickInterval speed) (always Tick)

        Paused ->
            Sub.none


tickInterval : Speed -> Time
tickInterval speed =
    case speed of
        Slow ->
            600 * millisecond

        Fast ->
            300 * millisecond



-- UTIL


leftArrow : KeyCode
leftArrow =
    37


rightArrow : KeyCode
rightArrow =
    39



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
