module Element.ThreeColumn exposing (..)

{-| Three columns, taking 33% of the parent container.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { first : Html msg
    , second : Html msg
    , third : Html msg
    }


element : Data msg -> Html msg
element data =
    div
        [ class "three columns" ]
        [ div
            [ class "column one" ]
            [ data.first ]
        , div
            [ class "column two" ]
            [ data.second ]
        , div
            [ class "column three" ]
            [ data.third ]
        ]
