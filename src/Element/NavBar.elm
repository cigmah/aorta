module Element.NavBar exposing (..)

{-| A basic navigation bar at the top of the page.

The brand item is included in this module and not exposed.

-}

import Element.SmallPopup as SmallPopup
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Icon as Icon


{-| Basic navigation bar data.
-}
type alias Data msg =
    { regularItems : List (RegularItemData msg) -- regular menu items, hidden on mobile
    , rightItem : RightItemData msg -- a single item on the right, shown on mobile
    }


{-| Basic navigation bar element
-}
element : Data msg -> Html msg
element data =
    nav
        [ id "nav-bar" ]
        [ ul
            [ id "nav-bar-list" ]
            [ li
                [ id "nav-bar-brand" ]
                [ div
                    [ id "nav-bar-brand-icon" ]
                    [ Icon.aortaWhite ]
                , div
                    [ id "nav-bar-brand-text" ]
                    [ text "AORTA" ]
                ]
            , li
                [ id "nav-bar-regular-item-list" ]
                [ ul
                    [ id "nav-bar-regular-item-list" ]
                    (List.map regularItemElement data.regularItems)
                ]
            , rightItemElement data.rightItem
            ]
        ]


{-| A regular item is a regular nav bar link, left-aligned.

These are hidden on mobile.

-}
type alias RegularItemData msg =
    { text : String
    , active : Bool
    , href : Attribute msg
    }


{-| A regular left-aligned item.
-}
regularItemElement : RegularItemData msg -> Html msg
regularItemElement data =
    li
        [ class "nav-bar-regular-item" ]
        [ a
            [ class "nav-bar-regular-item-link"
            , classList [ ( "active", data.active ) ]
            , data.href
            ]
            [ text data.text ]
        ]


{-| A right item is a nav bar link, right-aligned, with a popup.

This is visible on mobile.

-}
type alias RightItemData msg =
    { text : String
    , onClick : msg
    , smallPopup : Maybe (SmallPopup.Data msg)
    }


rightItemElement : RightItemData msg -> Html msg
rightItemElement data =
    let
        popup =
            data.smallPopup
                |> Maybe.map SmallPopup.element
                |> Maybe.withDefault (div [] [])
    in
    li
        [ class "nav-bar-right-item" ]
        [ button
            [ class "nav-bar-right-item-button"
            , onClick data.onClick
            ]
            [ text (data.text ++ " ▾") ]
        , popup
        ]
