module Msg exposing (Msg(..))

import Matrix exposing (Coordinate)
import Pattern exposing (Pattern)


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
    | ChangeSpeed
    | ChangeZoom
    | ChangeSize
    | ChangeTheme
    | NoOp
