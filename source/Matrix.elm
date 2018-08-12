module Matrix
    exposing
        ( Matrix
        , Coordinate
        , create
        , width
        , height
        , get
        , set
        , update
        , map
        , coordinateMap
        , foldl
        , all
        , toList
        , neighbours
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
create ({ width, height } as dimensions) value =
    Array.repeat (width * height) value
        |> Matrix dimensions


width : Matrix a -> Int
width (Matrix dimensions _) =
    dimensions.width


height : Matrix a -> Int
height (Matrix dimensions _) =
    dimensions.height


toIndex : Dimensions -> Coordinate -> Index
toIndex { width, height } { x, y } =
    let
        wrappedX =
            wrap 0 (width - 1) x

        wrappedY =
            wrap 0 (height - 1) y
    in
        (wrappedY * width) + wrappedX


wrap : Int -> Int -> Int -> Int
wrap min max value =
    if value < min then
        max
    else if value > max then
        min
    else
        value


toCoordinate : Dimensions -> Index -> Coordinate
toCoordinate { width, height } index =
    if index < width * height then
        { x = index % width
        , y = index // width
        }
    else
        { x = width - 1, y = height - 1 }


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
        |> List.filterMap ((flip get) matrix)


offsetBy : Coordinate -> ( Int, Int ) -> Coordinate
offsetBy { x, y } ( dx, dy ) =
    { x = x + dx
    , y = y + dy
    }
