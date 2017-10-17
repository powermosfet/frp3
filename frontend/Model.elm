module Model exposing (..)

import RemoteData exposing (WebData)


type alias Credentials =
    { username : String
    , password : String
    }


type alias Id =
    Int


type alias Category =
    { id : Id
    , name : String
    }


type alias BudgetData =
    { name : String
    }


type Budget
    = Budget Id BudgetData
    | NewBudget BudgetData


type alias BudgetItem =
    { id : Id
    , budget : Int
    , category : Int
    , amount : Float
    , frequenct : Int
    }


type alias Model =
    { budgets : WebData (List Budget) }
