module Element.GhostButton exposing (..)

{-| A ghost-styled button.

This is usually s secondary call-to-action on a page.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { text : String
    , onClick : msg
    }


element : Data msg -> Html msg
element data =
    button
        [ class "ghost-button"
        , type_ "button"
        , onClick data.onClick
        ]
        [ text data.text ]
