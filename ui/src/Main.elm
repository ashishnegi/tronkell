module Main exposing (main)

import Model exposing (Model)
import Message exposing (..)
import Update exposing (update)
import View exposing (view)
import Subscription exposing (subscriptions)

import Html.App as App

main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : String -> (Model, Cmd Msg)
init wsserver = (Model Nothing Nothing Nothing Nothing wsserver, Cmd.none)
