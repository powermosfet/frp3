module Model exposing (..)

import RemoteData exposing (WebData)


type alias Credentials =
    { username : String
    , password : String
    }


type alias Profile =
    { username : String
    , email : String
    }


type alias Password =
    String


type User
    = NewUser Password Profile
    | UserProfile Id Profile


type alias ApiError =
    { code : Int
    , message : String
    }


type alias ApiResult a =
    Result ApiError a


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
