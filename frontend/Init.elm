module Init exposing (..)

import Message exposing (Msg)
import Model exposing (Model, Route(RouteBudgets), Credentials)
import Command exposing (getProfile)
import RemoteData exposing (RemoteData(NotAsked))
import Navigation exposing (Location)
import Routing exposing (parseLocation)


initCredentials : Credentials
initCredentials =
    { username = ""
    , password = ""
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            parseLocation location
    in
        ( { loginCredentials = initCredentials
          , user = NotAsked
          , budgets = NotAsked
          , budgetName = ""
          , clicked = False
          , route = route
          , registerUsername = ""
          , registerEmail = ""
          , registerPassword = ""
          , registerRepeatPassword = ""
          }
        , getProfile
        )
