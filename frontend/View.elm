module View exposing (..)

import Html exposing (Html, node, div, h1, p, ul, li, text)
import Model exposing (Model)
import Message exposing (Msg)
import Css
import Styles.Styles as Styles


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ Css.compile [ Styles.css ] |> .css |> text ]
        , h1 [] [ text "servant-elm-poc" ]
        , p [] [ text "The API returned the following cats:" ]
        ]
