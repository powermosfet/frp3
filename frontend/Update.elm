module Update exposing (..)

import Message exposing (Msg(..), LoggedInMsg(..))
import Model exposing (..)
import Command exposing (..)
import RemoteData as RD
import Routing exposing (parseLocation)


only : Model -> ( Model, Cmd Msg )
only model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LocationChanged location ->
            let
                newRoute =
                    parseLocation location

                command =
                    case newRoute of
                        RouteBudgets ->
                            getBudgets

                        _ ->
                            Cmd.none
            in
                ( { model | route = newRoute }, command )

        LoginUsernameChanged username ->
            let
                oldCredentials =
                    model.loginCredentials

                newCredentials =
                    { oldCredentials | username = username }
            in
                only { model | loginCredentials = newCredentials }

        LoginPasswordChanged password ->
            let
                oldCredentials =
                    model.loginCredentials

                newCredentials =
                    { oldCredentials | password = password }
            in
                only { model | loginCredentials = newCredentials }

        RegisterUsernameChanged username ->
            only { model | registerUsername = username }

        RegisterEmailChanged email ->
            only { model | registerEmail = email }

        RegisterPasswordChanged password ->
            only { model | registerPassword = password }

        RegisterRepeatPasswordChanged password ->
            only { model | registerRepeatPassword = password }

        RegisterNewUser ->
            ( model
            , postRegister
                (NewUser model.registerPassword
                    { username = model.registerUsername
                    , email = model.registerEmail
                    }
                )
            )

        PostLogin ->
            ( model, postLogin model.loginCredentials )

        LoginResponse rdUser ->
            only { model | user = rdUser }

        LoggedIn loggedInMsg ->
            case model.user of
                RD.Success user ->
                    loggedInUpdate user loggedInMsg model

                _ ->
                    only model


loggedInUpdate : User -> LoggedInMsg -> Model -> ( Model, Cmd Msg )
loggedInUpdate user message model =
    case message of
        NewBudgets budgets ->
            only { model | budgets = budgets }

        BudgetCreated rdBudget ->
            only
                { model
                    | budgets = RD.map2 (::) rdBudget model.budgets
                    , budgetName = ""
                }

        BudgetNameChanged name ->
            only { model | budgetName = name }

        AddBudget ->
            ( model, postBudget user (NewBudget model.budgetName) )
