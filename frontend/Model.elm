module Model exposing (..)

import RemoteData exposing (WebData)
import Http


type Id
    = Id Int
    | New


type alias Category =
    { id : Id
    , name : String
    }


type alias Budget =
    { id : Id
    , name : String
    }


type alias BudgetItem =
    { id : Id
    , budget : Int
    , category : Int
    , amount : Float
    , frequenct : Int
    }


type alias Model =
    { budgets : WebData (List Budget) }
