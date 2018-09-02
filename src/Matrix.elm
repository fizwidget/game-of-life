module Matrix exposing
    ( Coordinate
    , Matrix
    , all
    , coordinateMap
    , create
    , foldl
    , get
    , height
    , map
    , neighbours
    , set
    , toList
    , update
    , width
    )

import Array exposing (Array)


type Matrix a
    = Matrix Dimensions (Array a)


type alias Dimensions =
    { width : Int
    , height : Int
    }


type alias Index =
    Int


type alias Coordinate =
    { x : Int
    , y : Int
    }


create : Dimensions -> a -> Matrix a
create dimensions value =
    Array.repeat (dimensions.width * dimensions.height) value
        |> Matrix dimensions


width : Matrix a -> Int
width (Matrix dimensions _) =
    dimensions.width


height : Matrix a -> Int
height (Matrix dimensions _) =
    dimensions.height


toIndex : Dimensions -> Coordinate -> Index
toIndex dimensions { x, y } =
    let
        wrappedX =
            wrap 0 (dimensions.width - 1) x

        wrappedY =
            wrap 0 (dimensions.height - 1) y
    in
    (wrappedY * dimensions.width) + wrappedX


wrap : Int -> Int -> Int -> Int
wrap min max value =
    if value < min then
        max

    else if value > max then
        min

    else
        value


toCoordinate : Dimensions -> Index -> Coordinate
toCoordinate dimensions index =
    if index < dimensions.width * dimensions.height then
        { x = modBy dimensions.width index
        , y = index // dimensions.width
        }

    else
        { x = dimensions.width - 1
        , y = dimensions.height - 1
        }


get : Coordinate -> Matrix a -> Maybe a
get coordinate (Matrix dimensions array) =
    Array.get (toIndex dimensions coordinate) array


set : Coordinate -> a -> Matrix a -> Matrix a
set coordinate value (Matrix dimensions array) =
    Array.set (toIndex dimensions coordinate) value array
        |> Matrix dimensions


update : (a -> a) -> Coordinate -> Matrix a -> Matrix a
update f coordinate matrix =
    matrix
        |> get coordinate
        |> Maybe.map f
        |> Maybe.map (\value -> set coordinate value matrix)
        |> Maybe.withDefault matrix


map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix dimensions array) =
    Array.map f array
        |> Matrix dimensions


coordinateMap : (Coordinate -> a -> b) -> Matrix a -> Matrix b
coordinateMap f (Matrix dimensions array) =
    Array.indexedMap (toCoordinate dimensions >> f) array
        |> Matrix dimensions


foldl : (a -> b -> b) -> b -> Matrix a -> b
foldl f initial (Matrix _ array) =
    Array.foldl f initial array


all : (a -> Bool) -> Matrix a -> Bool
all predicate (Matrix _ array) =
    array
        |> Array.filter (predicate >> not)
        |> Array.isEmpty


toList : Matrix a -> List a
toList (Matrix _ array) =
    Array.toList array


neighbours : Matrix a -> Coordinate -> List a
neighbours matrix coordinate =
    [ ( 1, 1 ), ( 1, 0 ), ( 1, -1 ), ( 0, -1 ), ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, 1 ) ]
        |> List.map (offsetBy coordinate)
        |> List.filterMap ((\b a -> get a b) matrix)


offsetBy : Coordinate -> ( Int, Int ) -> Coordinate
offsetBy { x, y } ( dx, dy ) =
    { x = x + dx
    , y = y + dy
    }