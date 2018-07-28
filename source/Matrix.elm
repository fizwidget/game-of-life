module Data.Matrix
    exposing
        ( Matrix
        , Coordinate
        , create
        , get
        , set
        , map
        , coordinateMap
        , getRows
        , getNeighbours
        )

import Array exposing (Array)


type Matrix a
    = Matrix (Array (Array a))


type alias Dimensions =
    { width : Int, height : Int }


type alias Coordinate =
    { x : Int, y : Int }


create : Dimensions -> a -> Matrix a
create { width, height } value =
    let
        row =
            Array.repeat width value

        columns =
            Array.repeat height row
    in
        Matrix columns


get : Matrix a -> Coordinate -> Maybe a
get (Matrix rows) { x, y } =
    rows
        |> Array.get y
        |> Maybe.andThen (Array.get x)


set : Coordinate -> a -> Matrix a -> Matrix a
set { x, y } value (Matrix rows) =
    rows
        |> Array.get y
        |> Maybe.map (Array.set x value)
        |> Maybe.map (\row -> Array.set y row rows)
        |> Maybe.map Matrix
        |> Maybe.withDefault (Matrix rows)


map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix rows) =
    Array.map (Array.map f) rows
        |> Matrix


coordinateMap : (Coordinate -> a -> b) -> Matrix a -> Matrix b
coordinateMap f (Matrix rows) =
    let
        mapRow y row =
            Array.indexedMap (\x -> \value -> f { x = x, y = y } value) row
    in
        Array.indexedMap mapRow rows
            |> Matrix


getRows : Matrix a -> List (List a)
getRows (Matrix rows) =
    rows
        |> Array.map (Array.toList)
        |> Array.toList


getNeighbours : Coordinate -> Matrix a -> List a
getNeighbours coordinate matrix =
    [ ( 1, 1 ), ( 1, 0 ), ( 1, -1 ), ( 0, -1 ), ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, 1 ) ]
        |> List.map (offsetBy coordinate)
        |> List.filterMap (get matrix)


offsetBy : Coordinate -> ( Int, Int ) -> Coordinate
offsetBy { x, y } ( dx, dy ) =
    { x = x + dx
    , y = y + dy
    }
