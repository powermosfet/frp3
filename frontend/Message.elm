module Message exposing (..)

import RemoteData exposing (WebData)
import Model exposing (..)
import Navigation exposing (Location)


type Msg
    = LocationChanged Location
    | LoginUsernameChanged String
    | LoginPasswordChanged String
    | RegisterUsernameChanged String
    | RegisterPasswordChanged String
    | RegisterRepeatPasswordChanged String
    | RegisterEmailChanged String
    | RegisterNewUser
    | PostLogin
    | LoginResponse (WebData User)
    | LoggedIn LoggedInMsg


type LoggedInMsg
    = NewBudgets (WebData (List Budget))
    | BudgetNameChanged String
    | AddBudget
    | BudgetCreated (WebData Budget)
