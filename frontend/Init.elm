module Init exposing (..)

import RemoteData exposing (RemoteData(NotAsked))
import Model exposing (Model)
import Message exposing (Msg)
import Command exposing (postLogin)


init : ( Model, Cmd Msg )
init =
    ( { budgets = NotAsked }, postLogin { username = "alf", password = "alfisbest" } )
