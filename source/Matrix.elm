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
        , equals
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
toIndex { width } { x, y } =
    (y * width) + x


toCoordinate : Dimensions -> Index -> Coordinate
toCoordinate { width } index =
    { x = index % width
    , y = index // width
    }


get : Matrix a -> Coordinate -> Maybe a
get (Matrix dimensions array) coordinate =
    let
        index =
            toIndex dimensions coordinate
    in
        Array.get index array


set : Coordinate -> a -> Matrix a -> Matrix a
set coordinate value (Matrix dimensions array) =
    let
        index =
            toIndex dimensions coordinate
    in
        Array.set index value array
            |> Matrix dimensions


update : Coordinate -> Matrix a -> (a -> a) -> Matrix a
update coordinate matrix f =
    coordinate
        |> get matrix
        |> Maybe.map f
        |> Maybe.map (\updatedValue -> set coordinate updatedValue matrix)
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


equals : Matrix a -> Matrix a -> Bool
equals (Matrix _ a) (Matrix _ b) =
    a == b


toList : Matrix a -> List ( Coordinate, a )
toList (Matrix dimensions array) =
    array
        |> Array.indexedMap (\index -> \value -> ( toCoordinate dimensions index, value ))
        |> Array.toList


offsetBy : Coordinate -> ( Int, Int ) -> Coordinate
offsetBy { x, y } ( dx, dy ) =
    { x = x + dx
    , y = y + dy
    }


neighbours : Coordinate -> Matrix a -> List a
neighbours coordinate matrix =
    [ ( 1, 1 ), ( 1, 0 ), ( 1, -1 ), ( 0, -1 ), ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, 1 ) ]
        |> List.map (offsetBy coordinate)
        |> List.filterMap (get matrix)
