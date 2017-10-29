module Init exposing (..)

import Message exposing (Msg(LocationChanged))
import Model exposing (Model, Route(RouteNotFound), Credentials)
import Command exposing (getProfile)
import RemoteData exposing (RemoteData(NotAsked))
import Navigation exposing (Location)
import Routing exposing (parseLocation)
import Update exposing (update)


initCredentials : Credentials
initCredentials =
    { username = ""
    , password = ""
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initModel =
            { loginCredentials = initCredentials
            , user = NotAsked
            , budgets = NotAsked
            , budgetName = ""
            , clicked = False
            , route = RouteNotFound
            , registerUsername = ""
            , registerEmail = ""
            , registerPassword = ""
            , registerRepeatPassword = ""
            }

        ( updateModel, updateCommand ) =
            update (LocationChanged location) initModel
    in
        ( updateModel
        , Cmd.batch [ getProfile, updateCommand ]
        )
