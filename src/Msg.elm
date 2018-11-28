module Msg exposing (Msg(..), Speed(..))

import Matrix exposing (Coordinate)
import Pattern exposing (Pattern)


type alias Milliseconds =
    Int


type Speed
    = Interval Milliseconds


type alias UserInput =
    String


type Msg
    = ClockTick
    | StepBack
    | StepForward
    | MouseDown Coordinate
    | MouseOver Coordinate
    | MouseUp
    | RandomPatternRequest
    | RandomPatternResponse Pattern
    | ImportFieldOpen
    | ImportFieldChange UserInput
    | ImportFieldCancel
    | ChangeStatus
    | OpenSpeedField
    | CloseSpeedField
    | ChangeSpeed Speed
    | ChangeZoom
    | ChangeSize
    | ChangeTheme
    | NoOp
