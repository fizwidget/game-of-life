module Pattern exposing
    ( Pattern
    , centerAt
    , height
    , parseLife106
    , toCoordinates
    , width
    )

import Maybe.Extra as Maybe


type alias Coordinate =
    { x : Int
    , y : Int
    }


type Pattern
    = Pattern (List Coordinate)


toCoordinates : Pattern -> List Coordinate
toCoordinates (Pattern pattern) =
    pattern


parseLife106 : String -> Maybe Pattern
parseLife106 text =
    String.lines text
        |> stripHeader
        |> List.map parseCoordinate
        |> Maybe.combine
        |> Maybe.map Pattern


stripHeader : List String -> List String
stripHeader lines =
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
    range (List.map .x coordinates)


height : Pattern -> Int
height (Pattern coordinates) =
    range (List.map .y coordinates)


range : List number -> number
range xs =
    let
        min =
            List.minimum xs |> Maybe.withDefault 0

        max =
            List.maximum xs |> Maybe.withDefault 0
    in
    max - min


centerAt : Coordinate -> Pattern -> Pattern
centerAt { x, y } (Pattern pattern) =
    let
        xs =
            List.map .x pattern

        ys =
            List.map .y pattern

        minX =
            List.minimum xs |> Maybe.withDefault 0

        minY =
            List.minimum ys |> Maybe.withDefault 0

        dx =
            x - minX - (width (Pattern pattern) // 2)

        dy =
            y - minY - (height (Pattern pattern) // 2)
    in
    Pattern (List.map (offsetBy dx dy) pattern)


offsetBy : Int -> Int -> Coordinate -> Coordinate
offsetBy dx dy { x, y } =
    { x = x + dx
    , y = y + dy
    }
