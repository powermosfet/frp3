module Command exposing (..)

import Http
import RemoteData
import Json.Decode exposing (..)
import Message exposing (Msg(..))
import Model exposing (Id(..), Budget)


maybeToId : Maybe Int -> Id
maybeToId mId =
    Maybe.map Id mId
        |> Maybe.withDefault New


idDecoder : Decoder Id
idDecoder =
    map (Maybe.map Id >> Maybe.withDefault New) (maybe (field "id" int))


budgetDecoder : Decoder Budget
budgetDecoder =
    map2 Budget idDecoder (field "name" string)


getBudgets : Cmd Msg
getBudgets =
    let
        url =
            "budget"
    in
        Http.get url (list budgetDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map NewBudgets
