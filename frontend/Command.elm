module Command exposing (..)

import Http
import RemoteData
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Extra exposing ((|:))
import Message exposing (Msg(..), LoggedInMsg(..))
import Model exposing (..)


entityDecoder : (Id -> a -> b) -> (a -> b) -> Decoder a -> Decoder b
entityDecoder entity new data =
    oneOf [ map2 entity (field "id" Decode.int) data, map new data ]


userDecoder : Decoder User
userDecoder =
    succeed User
        |: (field "id" Decode.int)
        |: (succeed Profile
                |: (field "username" Decode.string)
                |: (field "email" Decode.string)
           )


budgetDecoder : Decoder Budget
budgetDecoder =
    succeed Budget
        |: (field "id" Decode.int)
        |: (field "created" Json.Decode.Extra.date)
        |: (field "name" Decode.string)


getBudgets : Cmd Msg
getBudgets =
    let
        url =
            "budget"
    in
        Http.get url (Decode.list budgetDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map (LoggedIn << NewBudgets)


ownedEncoder : User -> (a -> List ( String, Encode.Value )) -> a -> Encode.Value
ownedEncoder owner subEncoder a =
    Encode.object (( "owner", Encode.int owner.id ) :: (subEncoder a))


budgetEncoder : Budget -> List ( String, Encode.Value )
budgetEncoder budget =
    case budget of
        Budget _ created name ->
            [ ( "name", Encode.string name )
            , ( "created", Encode.string (toString created) )
            ]

        NewBudget name ->
            [ ( "name", Encode.string name )
            ]


newUserEncoder : NewUser -> Encode.Value
newUserEncoder (NewUser password user) =
    Encode.object
        [ ( "username", Encode.string user.username )
        , ( "email", Encode.string user.email )
        , ( "password", Encode.string password )
        ]


credentialsEncoder : Credentials -> Encode.Value
credentialsEncoder credentials =
    Encode.object
        [ ( "username", Encode.string credentials.username )
        , ( "password", Encode.string credentials.password )
        ]


getProfile : Cmd Msg
getProfile =
    let
        url =
            "me"
    in
        Http.get url userDecoder
            |> RemoteData.sendRequest
            |> Cmd.map LoginResponse


postRegister : NewUser -> Cmd Msg
postRegister user =
    let
        url =
            "register"

        body =
            user
                |> newUserEncoder
                |> Http.jsonBody
    in
        Http.post url body userDecoder
            |> RemoteData.sendRequest
            |> Cmd.map LoginResponse


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
        Http.post url body userDecoder
            |> RemoteData.sendRequest
            |> Cmd.map LoginResponse


postBudget : User -> Budget -> Cmd Msg
postBudget owner budget =
    let
        url =
            "budget"

        body =
            budget
                |> (budgetEncoder >> Encode.object)
                |> Http.jsonBody
    in
        Http.post url body budgetDecoder
            |> RemoteData.sendRequest
            |> Cmd.map (LoggedIn << BudgetCreated)
