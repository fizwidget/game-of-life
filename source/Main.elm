module Main exposing (main)

import Html exposing (Html, div, text)
import Data.Matrix as Matrix exposing (Matrix)


type Msg
    = NoOp


type Cell
    = Alive
    | Dead


type alias Model =
    { matrix : Matrix Cell
    , iterations : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { matrix = Matrix.create { width = 10, height = 10 } Dead
      , iterations = 10
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html msg
view model =
    div [] [ text "Hi!" ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
