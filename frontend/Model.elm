module Model exposing (..)

import Http


type alias Entity a =
    { a | id : Int }


type alias Owned a =
    { a | owner : Int }


type alias Category =
    { name : String }


type alias Budget =
    { name : String }


type alias BudgetItem =
    { budget : Int
    , category : Int
    , amount : Float
    , frequenct : Int
    }


type alias Cat =
    { name : String }


type alias Model =
    { cats : List Cat }
