module Data.Matrix exposing (Matrix, create, get, set, map)

import Array exposing (Array)


type Matrix a
    = Matrix Dimensions (Array a)


type alias Dimensions =
    { width : Width
    , height : Height
    }


type alias Width =
    Int


type alias Height =
    Int


type alias Index =
    Int


type alias Coordinate =
    { x : Int
    , y : Int
    }


create : Dimensions -> a -> Matrix a
create { width, height } value =
    Array.repeat (width * height) value
        |> Matrix { width = width, height = height }


toIndex : Dimensions -> Coordinate -> Index
toIndex { width } { x, y } =
    (y * width) + x


get : Coordinate -> Matrix a -> Maybe a
get coordinate (Matrix dimensions array) =
    Array.get (toIndex dimensions coordinate) array


set : Coordinate -> Matrix a -> a -> Matrix a
set coordinate (Matrix dimensions array) value =
    Array.set (toIndex dimensions coordinate) value array
        |> Matrix dimensions


map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix dimensions array) =
    Array.map f array
        |> Matrix dimensions
