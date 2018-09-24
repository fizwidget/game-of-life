module Import exposing (Model, Msg, OutMsg(..), init, update, view)

import Cell exposing (Cell(..))
import Html.Styled exposing (Html, button, div, text, textarea)
import Html.Styled.Attributes exposing (css, value)
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
    | Confirm (Matrix Cell)


type OutMsg
    = ImportConfirmed (Matrix Cell)
    | NoOp


update : Msg -> Model -> ( Model, OutMsg )
update msg model =
    case msg of
        Change value ->
            ( { model | input = value }, NoOp )

        Confirm config ->
            ( model, ImportConfirmed config )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea [ value model.input, onInput Change ] []
        , decodeMatrix model.input
            |> Maybe.map viewConfirmButton
            |> Maybe.withDefault (div [] [])
        ]


viewConfirmButton : Matrix Cell -> Html Msg
viewConfirmButton config =
    button [ onClick (Confirm config) ] [ text "Confirm" ]



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

        emptyMatrix =
            Maybe.map (\size -> Matrix.create size Dead) matrixSize
    in
    Maybe.map2 initializeMatrix coordinates emptyMatrix


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
