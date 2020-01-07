module Element.TopNotification exposing (..)

{-| A small unobstrusive notification displayed at the top of the page.
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
    div
        [ class "top-notification-container" ]
        [ article
            [ class "top-notification"
            , onClick data.onClick
            ]
            [ text data.text ]
        ]
