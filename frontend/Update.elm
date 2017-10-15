module Update exposing (..)

import Message exposing (Msg(..))
import Model exposing (Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewBudgets budgets ->
            ( { model | budgets = budgets }, Cmd.none )
