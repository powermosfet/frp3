module Message exposing (..)

import Http
import RemoteData exposing (WebData)
import Model exposing (Budget)


type Msg
    = NewBudgets (WebData (List Budget))
    | LoginResponse (Result Http.Error { status : Int, message : String })
