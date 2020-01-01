module Element.PrimaryLink exposing (..)

{-| An anchor that looks like a button.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { text : String
    , href : Attribute msg
    }


element : Data msg -> Html msg
element data =
    a
        [ class "primary-link"
        , data.href
        ]
        [ text data.text ]
