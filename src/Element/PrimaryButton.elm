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
    , submit : Bool -- whether this should be a submit button on forms
    }


element : Data msg -> Html msg
element data =
    let
        buttonType =
            if data.submit then
                "submit"

            else
                "button"
    in
    button
        [ class "primary-button"
        , type_ buttonType
        , onClick data.onClick
        ]
        [ text data.text ]
