module Controls exposing
    ( ImportField(..)
    , Status(..)
    , UserInput
    , onKeyDown
    , view
    )

import Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes as Attributes exposing (autofocus, class, classList, placeholder, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Msg exposing (Msg(..), Speed(..))



-- TYPES


type Status
    = Paused
    | Playing


type ImportField
    = Closed
    | Open UserInput


type alias UserInput =
    String



-- VIEW


view : Status -> Speed -> Bool -> ImportField -> Html Msg
view status speed isSpeedFieldOpen importField =
    div []
        [ div [ class "control-panel" ]
            [ viewStatusButton status
            , viewBackButton
            , viewForwardButton
            , viewSpeedButton speed isSpeedFieldOpen
            , viewZoomButton
            , viewResizeButton
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
            viewButton
                { text = "Play"
                , tooltip = "Play simulation (P)"
                , onClick = ChangeStatus
                , attributes = [ class "play-button" ]
                }

        Playing ->
            viewButton
                { text = "Pause"
                , tooltip = "Pause simulation (P)"
                , onClick = ChangeStatus
                , attributes = []
                }


viewBackButton : Html Msg
viewBackButton =
    viewButton
        { text = "Back ⇦"
        , tooltip = "Back (←)"
        , onClick = StepBack
        , attributes = []
        }


viewForwardButton : Html Msg
viewForwardButton =
    viewButton
        { text = "Forward ⇨"
        , tooltip = "Forward (→)"
        , onClick = StepForward
        , attributes = []
        }


viewSpeedButton : Speed -> Bool -> Html Msg
viewSpeedButton interval isOpen =
    viewControl
        { text = "Speed 🏃\u{200D}♀️"
        , tooltip = "Speed (S)"
        , isOpen = isOpen
        , value = interval
        , onOpen = OpenSpeedField
        , onClose = CloseSpeedField
        , onChange = ChangeSpeed
        , attributes = []
        }


viewZoomButton : Html Msg
viewZoomButton =
    viewButton
        { text = "Zoom 🔬"
        , tooltip = "Zoom (Z)"
        , onClick = ChangeZoom
        , attributes = []
        }


viewResizeButton : Html Msg
viewResizeButton =
    viewButton
        { text = "Size 📐"
        , tooltip = "Resize (V)"
        , onClick = ChangeSize
        , attributes = []
        }


viewRandomizeButton : Html Msg
viewRandomizeButton =
    viewButton
        { text = "Randomize 🎲"
        , tooltip = "Randomize (R)"
        , onClick = RandomPatternRequest
        , attributes = []
        }


viewThemeButton : Html Msg
viewThemeButton =
    viewButton
        { text = "Theme 🎨"
        , tooltip = "Theme (T)"
        , onClick = ChangeTheme
        , attributes = []
        }


viewImportButton : ImportField -> Html Msg
viewImportButton importField =
    case importField of
        Closed ->
            viewButton
                { text = "Import ⬇"
                , tooltip = "Import pattern"
                , onClick = ImportFieldOpen
                , attributes = []
                }

        Open _ ->
            viewButton
                { text = "Cancel"
                , tooltip = "Cancel import"
                , onClick = ImportFieldCancel
                , attributes = []
                }


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


type alias ControlConfig =
    { text : String
    , tooltip : String
    , value : Speed
    , isOpen : Bool
    , onOpen : Msg
    , onClose : Msg
    , onChange : Speed -> Msg
    , attributes : List (Attribute Msg)
    }


type alias ButtonConfig =
    { text : String
    , tooltip : String
    , onClick : Msg
    , attributes : List (Attribute Msg)
    }


viewControl : ControlConfig -> Html Msg
viewControl { text, tooltip, value, isOpen, onOpen, onClose, onChange, attributes } =
    let
        (Interval interval) =
            value
    in
    div [ class "control" ]
        [ if isOpen then
            viewFlyout interval (Interval >> ChangeSpeed)

          else
            Html.text ""
        , viewButton
            { text = text
            , tooltip = tooltip
            , onClick =
                if isOpen then
                    onClose

                else
                    onOpen
            , attributes = attributes
            }
        ]


type alias SliderConfig =
    { value : Int
    , min : Int
    , max : Int
    , onChange : Int -> Msg
    }


viewSlider : SliderConfig -> Html Msg
viewSlider { value, min, max, onChange } =
    input
        [ type_ "range"
        , Attributes.min (String.fromInt min)
        , Attributes.max (String.fromInt max)
        , Attributes.value (String.fromInt value)
        , onInput (String.toInt >> Maybe.map onChange >> Maybe.withDefault NoOp)
        ]
        []


viewFlyout : Int -> (Int -> Msg) -> Html Msg
viewFlyout value onChange =
    div
        [ class "flyout" ]
        [ viewSlider
            { value = value
            , min = 0
            , max = 1000
            , onChange = onChange
            }
        ]


viewButton : ButtonConfig -> Html Msg
viewButton { text, tooltip, onClick, attributes } =
    let
        baseAttributes =
            [ class "button", title tooltip, Html.Events.onClick onClick ]
    in
    button (baseAttributes ++ attributes) [ Html.text text ]



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

        "r" ->
            RandomPatternRequest

        "z" ->
            ChangeZoom

        "v" ->
            ChangeSize

        "t" ->
            ChangeTheme

        _ ->
            NoOp
