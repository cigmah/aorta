module Element.PrimaryButton exposing (..)

{-| A primary-styled button.

This is usually the primary call-to-action on a page.

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
        [ class "primary-button"
        , type_ "button"
        , onClick data.onClick
        ]
        [ text data.text ]
