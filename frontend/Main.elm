module Main exposing (main)

import Navigation
import Html
import Model
import Init
import Message
import Update
import View


main : Program Never Model.Model Message.Msg
main =
    Navigation.program Message.LocationChanged
        { init = Init.init
        , view = View.view
        , update = Update.update
        , subscriptions = \_ -> Sub.none
        }
