module Matrix
    exposing
        ( Matrix
        , Coordinate
        , create
        , width
        , height
        , get
        , set
        , map
        , indexedMap
        , toListWithCoordinates
        , getRows
        , getNeighbours
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


map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix dimensions array) =
    Array.map f array
        |> Matrix dimensions


indexedMap : (Coordinate -> a -> b) -> Matrix a -> Matrix b
indexedMap f (Matrix dimensions array) =
    Array.indexedMap (toCoordinate dimensions >> f) array
        |> Matrix dimensions


toListWithCoordinates : Matrix a -> List ( Coordinate, a )
toListWithCoordinates (Matrix dimensions array) =
    array
        |> Array.indexedMap (\index -> \value -> ( toCoordinate dimensions index, value ))
        |> Array.toList


getRows : Matrix a -> List (List a)
getRows (Matrix dimensions array) =
    List.range 0 (dimensions.height - 1)
        |> List.map (\y -> Array.slice (y * dimensions.width) ((y + 1) * dimensions.width) array)
        |> List.map Array.toList


offsetBy : Coordinate -> ( Int, Int ) -> Coordinate
offsetBy { x, y } ( dx, dy ) =
    { x = x + dx
    , y = y + dy
    }


getNeighbours : Coordinate -> Matrix a -> List a
getNeighbours coordinate matrix =
    [ ( 1, 1 ), ( 1, 0 ), ( 1, -1 ), ( 0, -1 ), ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, 1 ) ]
        |> List.map (offsetBy coordinate)
        |> List.filterMap (get matrix)
