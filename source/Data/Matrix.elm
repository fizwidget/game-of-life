module Data.Matrix exposing (Matrix, create, get, set, map)

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


toIndex : Dimensions -> Coordinate -> Index
toIndex { width } { x, y } =
    (y * width) + x


get : Coordinate -> Matrix a -> Maybe a
get coordinate (Matrix dimensions array) =
    let
        index =
            toIndex dimensions coordinate
    in
        Array.get index array


set : Coordinate -> Matrix a -> a -> Matrix a
set coordinate (Matrix dimensions array) value =
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
