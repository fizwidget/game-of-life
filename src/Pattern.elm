module Pattern exposing
    ( Pattern
    , centerAt
    , generator
    , height
    , parseLife106Format
    , toCoordinates
    , width
    )

import Maybe.Extra as Maybe
import Random exposing (Generator)
import Result


type Pattern
    = Pattern (List Coordinate)


type alias Coordinate =
    { x : Int
    , y : Int
    }


toCoordinates : Pattern -> List Coordinate
toCoordinates (Pattern coordinates) =
    coordinates



-- PARSER


type ParseError
    = ParseError String


type alias PatternResult =
    Result ParseError Pattern


parseLife106Format : String -> PatternResult
parseLife106Format text =
    String.lines text
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> stripOptionalHeader
        |> List.map parseCoordinate
        |> Maybe.combine
        |> Maybe.filter (\coordinates -> List.length coordinates > 0)
        |> Maybe.map Pattern
        |> Result.fromMaybe (ParseError "Error parsing pattern")


stripOptionalHeader : List String -> List String
stripOptionalHeader lines =
    case lines of
        "#Life 1.06" :: tail ->
            tail

        _ ->
            lines


parseCoordinate : String -> Maybe Coordinate
parseCoordinate line =
    String.split " " line
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
    Maybe.map2
        Coordinate
        (String.toInt first)
        (String.toInt second)



-- RANDOM GENERATOR


type alias Dimensions =
    { width : Int
    , height : Int
    }


generator : Dimensions -> Generator Pattern
generator boundingBox =
    let
        coordinateCount =
            (boundingBox.width * boundingBox.height) // 4
    in
    Random.list coordinateCount (coordinateGenerator boundingBox)
        |> Random.map Pattern


coordinateGenerator : Dimensions -> Generator Coordinate
coordinateGenerator boundingBox =
    Random.map2
        Coordinate
        (Random.int 0 (boundingBox.width - 1))
        (Random.int 0 (boundingBox.height - 1))



-- UTILS


width : Pattern -> Int
width (Pattern coordinates) =
    maxDifference (List.map .x coordinates)


height : Pattern -> Int
height (Pattern coordinates) =
    maxDifference (List.map .y coordinates)


maxDifference : List number -> number
maxDifference xs =
    let
        min =
            List.minimum xs |> Maybe.withDefault 0

        max =
            List.maximum xs |> Maybe.withDefault 0
    in
    max - min


centerAt : Coordinate -> Pattern -> Pattern
centerAt center ((Pattern coordinates) as pattern) =
    let
        xCoordinates =
            List.map .x coordinates

        yCoordinates =
            List.map .y coordinates

        leftEdge =
            List.minimum xCoordinates
                |> Maybe.withDefault 0

        topEdge =
            List.minimum yCoordinates
                |> Maybe.withDefault 0

        xDistance =
            center.x - leftEdge - (width pattern // 2)

        yDistance =
            center.y - topEdge - (height pattern // 2)
    in
    List.map (offsetBy xDistance yDistance) coordinates
        |> Pattern


offsetBy : Int -> Int -> Coordinate -> Coordinate
offsetBy dx dy { x, y } =
    { x = x + dx
    , y = y + dy
    }
