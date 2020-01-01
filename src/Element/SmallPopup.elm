module Element.SmallPopup exposing (..)

{-| A small popup popups below the item on desktop, or as a modal on mobile.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { title : String
    , body : List (Html msg)
    , onClose : msg
    }


element : Data msg -> Html msg
element data =
    section
        [ class "small-popup-group" ]
        [ div
            [ class "modal-background"
            , onClick data.onClose
            ]
            []
        , article
            [ class "small-popup" ]
            [ header
                [ class "small-popup-header" ]
                [ div [ class "small-popup-header-title" ] [ text data.title ]
                , button [ class "small-popup-header-close-button", onClick data.onClose ] [ text "×" ]
                ]
            , section
                [ class "small-popup-body" ]
                data.body
            ]
        ]
