module Main exposing (main)

import Browser exposing (Document)
import Browser.Events as Events
import Css exposing (..)
import Css.Transitions as Transitions exposing (easeInOut, transition)
import History exposing (History)
import Html.Styled as Html exposing (Html, button, div, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, cols, css, disabled, placeholder, rows, value)
import Html.Styled.Events exposing (onClick, onInput, onMouseDown, onMouseEnter, onMouseUp)
import Json.Decode as Decode exposing (Decoder)
import Matrix exposing (Coordinate, Dimensions, Matrix)
import Maybe.Extra as Maybe
import Time



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


type ImportField
    = Open String
    | Closed


type alias Model =
    { status : Status
    , cells : History Cells
    , mouse : Mouse
    , speed : Speed
    , importField : ImportField
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { status = Paused
      , cells = History.begin initialCells
      , mouse = Up
      , speed = Slow
      , importField = Closed
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
    | KeyDown Key
    | OpenImportField
    | ImportFieldChange String


type Key
    = LeftKey
    | RightKey
    | PKey
    | OtherKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg ({ status, mouse, cells, importField } as model) =
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

        KeyDown key ->
            case key of
                LeftKey ->
                    undo model

                RightKey ->
                    redo model

                PKey ->
                    toggleStatus model

                OtherKey ->
                    model

        OpenImportField ->
            { model | importField = Open "" }

        ImportFieldChange input ->
            case decodePattern input of
                Just decodedPattern ->
                    { model | importField = Closed, cells = History.begin decodedPattern }

                Nothing ->
                    { model | importField = Open input }


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



-- DECODE


decodePattern : String -> Maybe (Matrix Cell)
decodePattern value =
    let
        coordinates =
            value
                |> String.lines
                |> dropHeader
                |> List.map decodeLine
                |> Maybe.combine

        -- Size is wrong :/
        matrixSize =
            coordinates
                |> Maybe.map patternSize
                |> Maybe.map (applyMinSize { width = 18, height = 18 })

        centeredCoordinates =
            Maybe.map2 centerPattern coordinates matrixSize

        emptyMatrix =
            Maybe.map (\size -> Matrix.create size Dead) matrixSize
    in
    Maybe.map2 initializeMatrix centeredCoordinates emptyMatrix


dropHeader : List String -> List String
dropHeader lines =
    case lines of
        "#Life 1.06" :: tail ->
            tail

        _ ->
            lines


decodeLine : String -> Maybe Coordinate
decodeLine line =
    line
        |> String.split " "
        |> toPair
        |> Maybe.andThen toCoordinate


toPair : List String -> Maybe ( String, String )
toPair values =
    case values of
        first :: second :: [] ->
            Just ( first, second )

        _ ->
            Nothing


toCoordinate : ( String, String ) -> Maybe Coordinate
toCoordinate ( first, second ) =
    let
        ( x, y ) =
            ( String.toInt first, String.toInt second )
    in
    Maybe.map2 Coordinate x y


patternSize : List Coordinate -> Dimensions
patternSize coordinates =
    { width = range (List.map .x coordinates)
    , height = range (List.map .y coordinates)
    }


centerPattern : List Coordinate -> Dimensions -> List Coordinate
centerPattern pattern dimensions =
    let
        midX =
            dimensions.width // 2

        -- Need to account for pattern not being zero'd
        patternWidth =
            range (List.map .x pattern)

        dx =
            midX - (patternWidth // 2)

        dy =
            5
    in
    List.map (offsetBy dx dy) pattern


applyMinSize : Dimensions -> Dimensions -> Dimensions
applyMinSize minDimensions requestedDimensions =
    { width = max minDimensions.width requestedDimensions.width
    , height = max minDimensions.height requestedDimensions.height
    }


range : List number -> number
range xs =
    let
        min =
            List.minimum xs |> Maybe.withDefault 0

        max =
            List.maximum xs |> Maybe.withDefault 0
    in
    max - min


offsetBy : Int -> Int -> Coordinate -> Coordinate
offsetBy dx dy { x, y } =
    { x = x + dx
    , y = y + dy
    }


initializeMatrix : List Coordinate -> Matrix Cell -> Matrix Cell
initializeMatrix coordinates matrix =
    List.foldl reduce matrix coordinates


reduce : Coordinate -> Matrix Cell -> Matrix Cell
reduce coordinate matrix =
    Matrix.set Alive coordinate matrix



-- VIEW


document : Model -> Document Msg
document model =
    { title = "Game of Life"
    , body = [ view model |> toUnstyled ]
    }


view : Model -> Html Msg
view { cells, status, speed, importField } =
    let
        currentCells =
            History.now cells

        transitionDuration =
            calculateTransitionDuration speed
    in
    div
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]
        [ viewCells transitionDuration currentCells |> squareContainer
        , viewControls status speed currentCells importField
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


viewCells : Milliseconds -> Cells -> Html Msg
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


viewCell : Milliseconds -> Percentage -> Coordinate -> Cell -> Html Msg
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


calculateTransitionDuration : Speed -> Milliseconds
calculateTransitionDuration speed =
    tickInterval speed + 200


cellSize : Cells -> Percentage
cellSize cells =
    100.0 / (Matrix.height cells |> toFloat)


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
            case ( modBy 2 x == 0, modBy 2 y == 0 ) of
                ( True, True ) ->
                    rgba 255 171 0 0.8

                ( True, False ) ->
                    rgba 54 179 126 0.8

                ( False, True ) ->
                    rgba 0 184 217 0.8

                ( False, False ) ->
                    rgba 101 84 192 0.8


viewControls : Status -> Speed -> Cells -> ImportField -> Html Msg
viewControls status speed cells importField =
    div []
        [ bottomLeft
            [ viewImportField importField
            , viewStatusButton status |> ifNotBlank cells
            , viewSpeedButton speed
            ]
        , bottomRight
            [ viewUndoButton status
            , viewRedoButton status
            ]
        ]


bottomLeft : List (Html msg) -> Html msg
bottomLeft =
    div
        [ css
            [ position fixed
            , left (px 20)
            , bottom (px 20)
            , displayFlex
            , flexDirection column
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


viewSpeedButton : Speed -> Html Msg
viewSpeedButton speed =
    case speed of
        Slow ->
            viewButton "Faster" (SetSpeed Fast) []

        Fast ->
            viewButton "Slower" (SetSpeed Slow) []


viewImportField : ImportField -> Html Msg
viewImportField importField =
    case importField of
        Open input ->
            textarea
                [ rows 32
                , cols 30
                , autofocus True
                , placeholder "Paste a 'Life 1.06' pattern here"
                , css [ borderRadius (px 4), resize none ]
                , value input
                , onInput ImportFieldChange
                ]
                []

        Closed ->
            viewButton "Import" OpenImportField []


viewUndoButton : Status -> Html Msg
viewUndoButton status =
    viewButton "⬅︎" Undo []


viewRedoButton : Status -> Html Msg
viewRedoButton status =
    viewButton "➡︎" Redo []


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
    , color (rgb 255 255 255)
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
        [ keyDownSubscription
        , tickSubscription status speed
        ]


tickSubscription : Status -> Speed -> Sub Msg
tickSubscription status speed =
    case status of
        Playing ->
            Time.every (tickInterval speed) (always Tick)

        Paused ->
            Sub.none


tickInterval : Speed -> Milliseconds
tickInterval speed =
    case speed of
        Slow ->
            600

        Fast ->
            300


keyDownSubscription : Sub Msg
keyDownSubscription =
    Events.onKeyDown keyDecoder
        |> Sub.map KeyDown


keyDecoder : Decoder Key
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map toKey


toKey : String -> Key
toKey value =
    case value of
        "ArrowLeft" ->
            LeftKey

        "ArrowRight" ->
            RightKey

        "p" ->
            PKey

        _ ->
            OtherKey


type alias Milliseconds =
    Float



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = document
        , update = update
        , subscriptions = subscriptions
        }
