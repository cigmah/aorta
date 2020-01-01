module Element.MainBody exposing (..)

{-| A <main> indicating the main document body of each page.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { id : String
    , children : List (Html msg)
    }


element : Data msg -> Html msg
element data =
    main_
        [ id data.id
        , class "main-body"
        ]
        data.children
