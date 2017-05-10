module Msgs exposing (..)

import Date exposing (..)
import Mouse exposing (Position)

type Msg =
    NoOp
    | Getnow Date
    --| Getwidth String
    | MouseDrop ClassMouse
    | DragAt Position
    | DragEnd Position

type alias ClassMouse =
    {className: String, position: Position}