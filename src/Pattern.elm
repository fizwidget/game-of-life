module Pattern exposing
    ( Pattern
    , centerAt
    , height
    , parseLife106
    , toCoordinates
    , width
    )

import Maybe.Extra as Maybe


type Pattern
    = Pattern (List Coordinate)


type alias Coordinate =
    { x : Int
    , y : Int
    }


toCoordinates : Pattern -> List Coordinate
toCoordinates (Pattern pattern) =
    pattern


parseLife106 : String -> Maybe Pattern
parseLife106 text =
    String.lines text
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> stripOptionalHeader
        |> List.map parseCoordinate
        |> Maybe.combine
        |> Maybe.map Pattern


stripOptionalHeader : List String -> List String
stripOptionalHeader lines =
    case lines of
        "#Life 1.06" :: tail ->
            tail

        _ ->
            lines


parseCoordinate : String -> Maybe Coordinate
parseCoordinate line =
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
    Maybe.map2
        Coordinate
        (String.toInt first)
        (String.toInt second)


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
