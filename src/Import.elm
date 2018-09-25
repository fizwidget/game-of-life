module Import exposing (Model, Msg, OutMsg(..), init, update, view)

import Cell exposing (Cell(..))
import Css exposing (..)
import Html.Styled exposing (Html, button, div, text, textarea)
import Html.Styled.Attributes exposing (autofocus, cols, css, disabled, placeholder, rows, value)
import Html.Styled.Events exposing (onClick, onInput)
import Matrix exposing (Coordinate, Dimensions, Matrix)
import Maybe.Extra as Maybe



-- MODEL


type alias Model =
    { input : String }



-- INIT


init : Model
init =
    { input = "" }



-- UPDATE


type Msg
    = Change String


type OutMsg
    = ImportConfirmed (Matrix Cell)
    | NoOp


update : Msg -> Model -> ( Model, OutMsg )
update msg model =
    case msg of
        Change value ->
            ( { model | input = value }
            , decodeMatrix value |> Maybe.map ImportConfirmed |> Maybe.withDefault NoOp
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ rows 32
            , cols 30
            , autofocus True
            , placeholder "Paste a Life 1.06 file here"
            , css [ borderRadius (px 4), resize none ]
            , value model.input
            , onInput Change
            ]
            []
        ]



-- DECODERS


decodeMatrix : String -> Maybe (Matrix Cell)
decodeMatrix value =
    let
        coordinates =
            value
                |> String.lines
                |> dropHeader
                |> List.map decodeLine
                |> Maybe.combine
                |> Maybe.map startAtZero

        matrixSize =
            coordinates
                |> Maybe.andThen calculateSize
                |> Maybe.map (applyMinSize { width = 18, height = 18 })

        centeredCoordinates =
            Maybe.map2 center coordinates matrixSize

        emptyMatrix =
            Maybe.map (\size -> Matrix.create size Dead) matrixSize
    in
    Maybe.map2 initializeMatrix centeredCoordinates emptyMatrix


center : List Coordinate -> Dimensions -> List Coordinate
center coordinates dimensions =
    let
        midX =
            dimensions.width // 2

        dx =
            midX - (patternWidth coordinates // 2)

        dy =
            3
    in
    List.map (offsetBy dx dy) coordinates


patternWidth : List Coordinate -> Int
patternWidth coordinates =
    coordinates
        |> List.map .x
        |> List.maximum
        |> Maybe.withDefault 0


applyMinSize : Dimensions -> Dimensions -> Dimensions
applyMinSize minDimensions requestedDimensions =
    { width = max minDimensions.width requestedDimensions.width
    , height = max minDimensions.height requestedDimensions.height
    }


startAtZero : List Coordinate -> List Coordinate
startAtZero coordinates =
    let
        minX =
            coordinates
                |> List.map .x
                |> (\xs -> 0 :: xs)
                |> List.minimum
                |> Maybe.withDefault 0

        minY =
            coordinates
                |> List.map .y
                |> (\xs -> 0 :: xs)
                |> List.minimum
                |> Maybe.withDefault 0
    in
    coordinates
        |> List.map (offsetBy -minX -minY)


offsetBy : Int -> Int -> Coordinate -> Coordinate
offsetBy dx dy { x, y } =
    { x = x + dx
    , y = y + dy
    }


dropHeader : List String -> List String
dropHeader lines =
    case lines of
        "#Life 1.06" :: tail ->
            tail

        _ ->
            lines


initializeMatrix : List Coordinate -> Matrix Cell -> Matrix Cell
initializeMatrix coordinates matrix =
    List.foldl reduce matrix coordinates


reduce : Coordinate -> Matrix Cell -> Matrix Cell
reduce coordinate matrix =
    Matrix.set Alive coordinate matrix


calculateSize : List Coordinate -> Maybe Dimensions
calculateSize coordinates =
    let
        width =
            List.maximum (List.map .x coordinates)

        height =
            List.maximum (List.map .y coordinates)
    in
    Maybe.map2 Dimensions width height


decodeLine : String -> Maybe Coordinate
decodeLine line =
    line
        |> String.split " "
        |> toPair
        |> Maybe.andThen toCoordinate


toPair : List String -> Maybe ( String, String )
toPair values =
    case values of
        first :: second :: _ ->
            Just ( first, second )

        _ ->
            Nothing


toCoordinate : ( String, String ) -> Maybe Coordinate
toCoordinate ( first, second ) =
    let
        x =
            String.toInt first

        y =
            String.toInt second
    in
    Maybe.map2 Coordinate x y
