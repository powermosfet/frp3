module Model exposing (..)

import RemoteData exposing (WebData)
import Date exposing (Date)


type alias Credentials =
    { username : String
    , password : String
    }


type alias User =
    { id : Id
    , profile : Profile
    }


type alias Profile =
    { username : String
    , email : String
    }


type NewUser
    = NewUser Password Profile


type alias Password =
    String


type alias ErrorCode =
    Int


type alias ErrorMessage =
    String


type alias Id =
    Int


type alias Category =
    { id : Id
    , name : String
    }


type alias Name =
    String


type Budget
    = Budget Id Date Name
    | NewBudget Name


type alias BudgetItem =
    { id : Id
    , budget : Int
    , category : Int
    , amount : Float
    , frequenct : Int
    }


type Route
    = RouteRegister
    | RouteLogin
    | RouteProfile
    | RouteBudgets
    | RouteBudget Id
    | RouteCategories
    | RouteCategory Id
    | RouteTransactions
    | RouteAddBudget
    | RouteNotFound


type alias Model =
    { loginCredentials : Credentials
    , user : WebData User
    , budgets : WebData (List Budget)
    , budgetName : String
    , clicked : Bool
    , route : Route
    , registerUsername : String
    , registerEmail : String
    , registerPassword : String
    , registerRepeatPassword : String
    }
