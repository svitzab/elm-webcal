module Main exposing (..)

import Date exposing (..)
import Html exposing (program)
import Model exposing (..)
import Mouse exposing (..)
import Msgs exposing (..)
import Update exposing (..)
import View exposing (view)
import Task exposing (..)

init : (Model, Cmd Msg)
init =
    (initialModel, now)

now : Cmd Msg
now = 
    Task.perform Getnow Date.now

-- MAIN

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragStart of
        Nothing ->
            Sub.none
        Just _ ->
            Sub.batch [
            Mouse.moves DragAt
            , Mouse.ups DragEnd
            ]