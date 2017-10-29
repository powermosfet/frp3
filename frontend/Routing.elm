module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)
import Model exposing (Route(..))


url : Route -> String
url route =
    "#"
        ++ case route of
            RouteBudgets ->
                "budget"

            RouteBudget budget ->
                "budget/" ++ (toString budget)

            RouteRegister ->
                "register"

            RouteLogin ->
                "login"

            RouteProfile ->
                "me"

            RouteCategories ->
                "category"

            RouteCategory category ->
                "category/" ++ (toString category)

            RouteTransactions ->
                "transaction"

            RouteAddBudget ->
                "budget/new"

            RouteNotFound ->
                "404"


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map RouteBudgets top
        , map RouteBudget (s "budget" </> int)
        , map RouteBudgets (s "budget")
        , map RouteRegister (s "register")
        , map RouteLogin (s "login")
        , map RouteProfile (s "me")
        , map RouteCategories (s "category")
        , map RouteCategory (s "category" </> int)
        , map RouteTransactions (s "transaction")
        , map RouteAddBudget (s "budget/new")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            RouteNotFound
