module Data.Matrix exposing (Matrix, create, get, set, map)

import Array exposing (Array)


type Matrix a
    = Matrix (Array a)


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


create : Width -> Height -> a -> Matrix a
create width height value =
    Array.repeat (width * height) value
        |> Matrix


toIndex : Coordinate -> Index
toIndex { x, y } =
    x * y


get : Coordinate -> Matrix a -> Maybe a
get coordinate (Matrix array) =
    Array.get (toIndex coordinate) array


set : Coordinate -> Matrix a -> a -> Matrix a
set coordinate (Matrix array) value =
    Array.set (toIndex coordinate) value array
        |> Matrix


map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix array) =
    Array.map f array
        |> Matrix
