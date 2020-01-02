module Element.Empty exposing (..)

{-| Simply an empty div.
-}

import Html exposing (Html, div)


element : Html msg
element =
    div [] []
