module View exposing (..)

import Html exposing (Html)
import Model exposing (..)
import Message exposing (Msg(..), LoggedInMsg(..))
import Element exposing (..)
import Element.Input as Input
import Element.Attributes as Attribute
import Element.Events as Event
import Stylesheet exposing (..)
import RemoteData
import Date exposing (Date, day, month, Month, year)
import Routing exposing (url)


menuItem currentRoute toRoute label =
    let
        style =
            if currentRoute == toRoute then
                MenuItem Selected
            else
                MenuItem NotSelected
    in
        link (url toRoute) <|
            el style
                [ Attribute.alignBottom
                , Attribute.padding 10
                ]
                (text label)


view : Model -> Html Msg
view model =
    viewport
        stylesheet
        (column MainStyle
            []
            [ row HeaderBar
                [ Attribute.spread
                , Attribute.paddingLeft 20
                , Attribute.paddingRight 20
                ]
                [ row MenuBar
                    [ Attribute.spacing 15 ]
                    [ menuItem model.route RouteCategories "Categories"
                    , menuItem model.route RouteBudgets "Budgets"
                    , menuItem model.route RouteTransactions "Transactions"
                    ]
                , viewUserMenu model
                ]
            , viewMain model
            ]
        )


viewUserMenu : Model -> Element FrpStyle variation Msg
viewUserMenu model =
    let
        menuLabel =
            case model.user of
                RemoteData.Success user ->
                    user.profile.username

                RemoteData.Loading ->
                    "..."

                _ ->
                    "Log in"
    in
        link (url RouteLogin) <|
            el (MenuItem NotSelected)
                [ Attribute.alignBottom
                , Attribute.padding 10
                ]
                (text menuLabel)


viewMain : Model -> Element FrpStyle variation Msg
viewMain model =
    case model.route of
        RouteLogin ->
            viewLogin model

        RouteRegister ->
            viewRegister model

        RouteBudgets ->
            Element.map LoggedIn (viewBudgets model)

        RouteAddBudget ->
            Element.map LoggedIn (viewAddBudget model)

        _ ->
            el NoStyle [] (text "NOT IMPLEMENTED!")


viewLogin : Model -> Element FrpStyle variation Msg
viewLogin model =
    column NoStyle
        [ Attribute.center
        ]
        [ h1 Title
            [ Attribute.paddingTop 15
            , Attribute.paddingBottom 25
            ]
            (text "Log in")
        , el CredentialBox
            [ Attribute.width (Attribute.px 400)
            ]
            (column NoStyle
                [ Attribute.padding 20
                , Attribute.spacing 20
                ]
                [ Input.username LoginField
                    []
                    { onChange = LoginUsernameChanged
                    , value = model.loginCredentials.username
                    , label = Input.labelLeft (text "Username")
                    , options = []
                    }
                , Input.currentPassword LoginField
                    []
                    { onChange = LoginPasswordChanged
                    , value = model.loginCredentials.password
                    , label = Input.labelLeft (text "Password")
                    , options = []
                    }
                , row NoStyle
                    [ Attribute.spread ]
                    [ link (url RouteRegister) <| button NoStyle [ Attribute.padding 5 ] (text "Register")
                    , button NoStyle [ Attribute.padding 5, Event.onClick PostLogin ] (text "Log in")
                    ]
                ]
            )
        ]


viewRegister : Model -> Element FrpStyle variation Msg
viewRegister model =
    column NoStyle
        [ Attribute.center
        ]
        [ h1 Title
            [ Attribute.paddingTop 15
            , Attribute.paddingBottom 25
            ]
            (text "Register user")
        , el CredentialBox
            [ Attribute.width (Attribute.px 400)
            ]
            (column NoStyle
                [ Attribute.padding 20
                , Attribute.spacing 20
                ]
                [ Input.username LoginField
                    []
                    { onChange = RegisterUsernameChanged
                    , value = model.loginCredentials.username
                    , label = Input.labelLeft (text "Username")
                    , options = []
                    }
                , Input.text LoginField
                    []
                    { onChange = RegisterEmailChanged
                    , value = model.loginCredentials.username
                    , label = Input.labelLeft (text "Username")
                    , options = []
                    }
                , Input.newPassword LoginField
                    []
                    { onChange = RegisterPasswordChanged
                    , value = model.loginCredentials.password
                    , label = Input.labelLeft (text "Password")
                    , options = []
                    }
                , Input.newPassword LoginField
                    []
                    { onChange = RegisterRepeatPasswordChanged
                    , value = model.loginCredentials.password
                    , label = Input.labelLeft (text "Repeat password")
                    , options = []
                    }
                , row NoStyle
                    [ Attribute.alignRight ]
                    [ button NoStyle [ Attribute.padding 5, Event.onClick RegisterNewUser ] (text "Register") ]
                ]
            )
        ]


viewAddBudget : Model -> Element FrpStyle variation LoggedInMsg
viewAddBudget model =
    column NoStyle
        []
        [ h1 Title
            [ Attribute.paddingTop 15
            , Attribute.paddingBottom 25
            ]
            (text "Add budget")
        , el NoStyle
            [ Attribute.width (Attribute.px 400)
            ]
            (column NoStyle
                [ Attribute.padding 20
                , Attribute.spacing 20
                ]
                [ Input.text LoginField
                    []
                    { onChange = BudgetNameChanged
                    , value = model.budgetName
                    , label = Input.labelLeft (text "Name")
                    , options = []
                    }
                , row NoStyle
                    [ Attribute.alignRight ]
                    [ button NoStyle [ Attribute.padding 5, Event.onClick AddBudget ] (text "Submit") ]
                ]
            )
        ]


standardTableHeader : String -> Element FrpStyle variation Msg
standardTableHeader h =
    el TableHeader [ Attribute.center, Attribute.padding 10 ] (text h)


standardTableCell : String -> Element FrpStyle variation Msg
standardTableCell contents =
    el TableCell [ Attribute.padding 10 ] (text contents)


viewBudgets : Model -> Element FrpStyle variation LoggedInMsg
viewBudgets model =
    let
        message t =
            [ text t ]

        budgetBoxes =
            case model.budgets of
                RemoteData.NotAsked ->
                    message "Initializing"

                RemoteData.Loading ->
                    message "Loading..."

                RemoteData.Failure _ ->
                    message "Oh, no! something went terribly wrong!"

                RemoteData.Success budgets ->
                    let
                        getBudgetData budget =
                            case budget of
                                Budget id created name ->
                                    { name = name, created = viewDate created, id = Just id }

                                NewBudget name ->
                                    { name = name, created = "", id = Nothing }

                        maybeLink mId =
                            case mId of
                                Just id ->
                                    link (url (RouteBudget id))

                                _ ->
                                    \x -> x
                    in
                        (List.map
                            (getBudgetData
                                >> \b ->
                                    maybeLink b.id <|
                                        column ItemBox
                                            [ Attribute.padding 10, Attribute.spacing 10, Attribute.width (Attribute.px 200) ]
                                            [ el ItemBoxHeading [] (text b.name)
                                            , el NoStyle [] (text b.created)
                                            ]
                            )
                            budgets
                        )
                            ++ [ column ItemBoxAdd
                                    [ Attribute.padding 10, Attribute.spacing 10 ]
                                    [ row NoStyle
                                        [ Attribute.spacing 10 ]
                                        [ Input.text LoginField
                                            [ Attribute.padding 5 ]
                                            { onChange = BudgetNameChanged
                                            , value = model.budgetName
                                            , label = Input.hiddenLabel "Name"
                                            , options = []
                                            }
                                        , button NoStyle [ Attribute.padding 5, Event.onClick AddBudget ] (text "Add")
                                        ]
                                    ]
                               ]
    in
        column NoStyle
            [ Attribute.padding 25 ]
            [ h1 Title
                [ Attribute.paddingTop 15
                , Attribute.paddingBottom 25
                ]
                (text "Budgets")
            , wrappedRow NoStyle [ Attribute.spacing 20 ] budgetBoxes
            ]


viewDate : Date -> String
viewDate date =
    let
        d =
            day date |> toString

        m =
            month date |> toString

        y =
            year date |> toString
    in
        d ++ ". " ++ m ++ " " ++ y
