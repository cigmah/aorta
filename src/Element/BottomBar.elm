module Element.BottomBar exposing (..)

{-| A bottom navigation bar, only shown on mobile.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Icon as Icon


type alias Data msg =
    { children : List (ItemData msg) }


type alias ItemData msg =
    { href : Attribute msg
    , text : String
    , active : Bool
    , icon : Html msg
    }


element : Data msg -> Html msg
element data =
    nav
        [ id "bottom-bar" ]
        [ ul
            [ id "bottom-bar-list" ]
            (List.map item data.children)
        ]


item : ItemData msg -> Html msg
item data =
    li
        [ class "bottom-bar-item" ]
        [ a
            [ class "bottom-bar-item-link"
            , data.href
            , classList [ ( "active", data.active ) ]
            ]
            [ div
                [ class "bottom-bar-item-icon" ]
                [ data.icon ]
            , div
                [ class "bottom-bar-item-text" ]
                [ text data.text ]
            ]
        ]
