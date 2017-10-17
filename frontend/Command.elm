module Command exposing (..)

import Http
import RemoteData
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Extra exposing ((|:))
import Message exposing (Msg(..))
import Model exposing (..)


entityDecoder : (Id -> a -> b) -> (a -> b) -> Decoder a -> Decoder b
entityDecoder entity new data =
    Decode.oneOf [ Decode.map2 entity (Decode.field "id" Decode.int) data, Decode.map new data ]


apiErrorDecoder : Decoder ApiError
apiErrorDecoder =
    (Decode.field "error"
        (Decode.succeed ApiError
            |: (Decode.field "code" Decode.int)
            |: (Decode.field "message" Decode.string)
        )
    )


userDecoder : Decoder User
userDecoder =
    Decode.succeed (UserProfile 3 (Profile "alf" "alf@alfie.com"))


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


apiErrorOrProfileDecoder : Decoder (ApiResult User)
apiErrorOrProfileDecoder =
    Decode.oneOf
        [ Decode.map Ok userDecoder
        , Decode.map Err apiErrorDecoder
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
