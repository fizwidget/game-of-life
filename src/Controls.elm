module Controls exposing
    ( ImportField(..)
    , Msg(..)
    , Speed(..)
    , Status(..)
    , UserInput
    , onKeyDown
    , view
    )

import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (autofocus, class, classList, placeholder, title, value)
import Html.Events exposing (onClick, onInput)



-- TYPES


type Status
    = Paused
    | Playing


type Speed
    = Slow
    | Medium
    | Fast


type ImportField
    = Closed
    | Open UserInput


type alias UserInput =
    String


type Msg
    = StepBack
    | StepForward
    | ImportFieldOpen
    | ImportFieldChange UserInput
    | ImportFieldCancel
    | RandomPatternRequest
    | ChangeStatus
    | ChangeSpeed
    | ChangeZoom
    | ChangeTheme
    | NoOp



-- VIEW


view : Status -> ImportField -> Html Msg
view status importField =
    div []
        [ div [ class "control-panel" ]
            [ viewStatusButton status
            , viewBackButton
            , viewForwardButton
            , viewSpeedButton
            , viewZoomButton
            , viewRandomizeButton
            , viewThemeButton
            , viewImportButton importField
            ]
        , viewImportField importField
        ]


viewStatusButton : Status -> Html Msg
viewStatusButton status =
    case status of
        Paused ->
            viewButton "Start" "Start simulation (P)" ChangeStatus [ class "play-button" ]

        Playing ->
            viewButton "Stop" "Stop simulation (P)" ChangeStatus []


viewBackButton : Html Msg
viewBackButton =
    viewButton "⇦" "Back (←)" StepBack []


viewForwardButton : Html Msg
viewForwardButton =
    viewButton "⇨" "Forward (→)" StepForward []


viewSpeedButton : Html Msg
viewSpeedButton =
    viewButton "🏃\u{200D}♀️" "Speed (S)" ChangeSpeed []


viewZoomButton : Html Msg
viewZoomButton =
    viewButton "🔬" "Zoom (Z)" ChangeZoom []


viewRandomizeButton : Html Msg
viewRandomizeButton =
    viewButton "🎲" "Randomize (R)" RandomPatternRequest []


viewThemeButton : Html Msg
viewThemeButton =
    viewButton "🎨" "Theme (T)" ChangeTheme []


viewImportButton : ImportField -> Html Msg
viewImportButton importField =
    case importField of
        Closed ->
            viewButton "Import" "Import pattern" ImportFieldOpen []

        Open _ ->
            viewButton "Cancel" "Cancel import" ImportFieldCancel []


viewImportField : ImportField -> Html Msg
viewImportField importField =
    case importField of
        Closed ->
            text ""

        Open userInput ->
            textarea
                [ autofocus True
                , placeholder "Paste a 'Life 1.06' pattern here..."
                , classList
                    [ ( "import-field", True )
                    , ( "invalid", not <| String.isEmpty userInput )
                    ]
                , value userInput
                , onInput ImportFieldChange
                ]
                []


viewButton : String -> String -> Msg -> List (Attribute Msg) -> Html Msg
viewButton description tooltip clickMsg extraAttributes =
    let
        allAttributes =
            [ class "button", title tooltip, onClick clickMsg ] ++ extraAttributes
    in
    button allAttributes [ text description ]



-- KEYBOARD


type alias Key =
    String


onKeyDown : Key -> Msg
onKeyDown key =
    case key of
        "ArrowLeft" ->
            StepBack

        "ArrowRight" ->
            StepForward

        "p" ->
            ChangeStatus

        "s" ->
            ChangeSpeed

        "r" ->
            RandomPatternRequest

        "z" ->
            ChangeZoom

        "t" ->
            ChangeTheme

        _ ->
            NoOp
