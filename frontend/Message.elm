module Message exposing (..)

import RemoteData exposing (WebData)
import Model exposing (Budget)


type Msg
    = NewBudgets (WebData (List Budget))
