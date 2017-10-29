module Stylesheet exposing (..)

import Color as C
import Style
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


type Selected
    = Selected
    | NotSelected


type FrpStyle
    = NoStyle
    | MainStyle
    | HeaderBar
    | MenuBar
    | MenuItem Selected
    | Title
    | Left
    | CredentialBox
    | LoginField
    | StandardTable
    | TableHeader
    | TableCell
    | ItemBox
    | ItemBoxHeading
    | ItemBoxAdd


palette =
    { color1 =
        C.rgb 255 191 0
    , color2 =
        C.rgb 232 63 111
    , color3 =
        C.rgb 34 116 165
    , color4 =
        C.rgb 50 147 111
    , color5 =
        C.rgb 255 255 255
    , transparent =
        C.rgba 0 0 0 0
    , black =
        C.rgb 0 0 0
    }


stylesheet : Style.StyleSheet FrpStyle variation
stylesheet =
    Style.styleSheet
        [ Style.style NoStyle []
        , Style.style MainStyle
            [ Font.typeface [ Font.sansSerif ] ]
        , Style.style Left
            [ Border.right 2 ]
        , Style.style HeaderBar
            [ Color.background palette.color3
            ]
        , Style.style MenuBar
            []
        , Style.style (MenuItem NotSelected)
            [ Color.text palette.color5
            , Color.background palette.transparent
            , Border.bottom 4
            , Border.top 4
            , Color.border palette.transparent
            ]
        , Style.style (MenuItem Selected)
            [ Color.text palette.color5
            , Color.background palette.transparent
            , Border.bottom 4
            , Color.border palette.color5
            ]
        , Style.style Title
            [ Font.size 28
            ]
        , Style.style CredentialBox
            []
        , Style.style LoginField
            [ Border.all 1
            , Color.border C.black
            , Border.rounded 3
            ]
        , Style.style StandardTable
            [ Border.all 1
            ]
        , Style.style TableHeader
            [ Font.bold
            , Font.underline
            , Color.background palette.color1
            ]
        , Style.style TableCell
            []
        , Style.style ItemBox
            [ Border.rounded 10
            , Border.all 2
            , Color.border palette.black
            , Color.background palette.transparent
            ]
        , Style.style ItemBoxHeading
            [ Font.bold
            ]
        , Style.style ItemBoxAdd
            [ Border.rounded 10
            , Border.all 3
            , Border.dashed
            , Color.border palette.black
            , Color.background palette.transparent
            ]
        ]
