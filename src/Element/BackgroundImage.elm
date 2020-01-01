module Element.BackgroundImage exposing (..)

{-| A full viewport background image.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


element : String -> Html msg
element imageSrc =
    img
        [ src imageSrc
        , class "background-image"
        ]
        []
