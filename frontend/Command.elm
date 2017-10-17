module Command exposing (..)

import Http
import RemoteData
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Extra exposing ((|:))
import Message exposing (Msg(..))
import Model exposing (Id, Budget(..), BudgetData, Credentials)


entityDecoder : (Id -> a -> b) -> (a -> b) -> Decoder a -> Decoder b
entityDecoder entity new data =
    Decode.oneOf [ Decode.map2 entity (Decode.field "id" Decode.int) data, Decode.map new data ]


budgetDecoder : Decoder Budget
budgetDecoder =
    entityDecoder
        Budget
        NewBudget
        (Decode.succeed BudgetData
            |: (Decode.field "name" Decode.string)
        )


getBudgets : Cmd Msg
getBudgets =
    let
        url =
            "budget"
    in
        Http.get url (Decode.list budgetDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map NewBudgets


credentialsEncoder : Credentials -> Encode.Value
credentialsEncoder credentials =
    Encode.object
        [ ( "username", Encode.string credentials.username )
        , ( "password", Encode.string credentials.password )
        ]


loginResponseDecoder : Decoder { status : Int, message : String }
loginResponseDecoder =
    Decode.map2 (\a b -> { status = a, message = b }) (Decode.field "status" Decode.int) (Decode.field "message" Decode.string)


postLogin : Credentials -> Cmd Msg
postLogin credentials =
    let
        url =
            "login"

        body =
            credentials
                |> credentialsEncoder
                |> Http.jsonBody
    in
        Http.post url body loginResponseDecoder
            |> Http.send LoginResponse
