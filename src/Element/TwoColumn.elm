module Element.TwoColumn exposing (..)

{-| Two columns, taking 50% of the parent container.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { first : Html msg
    , second : Html msg
    }


element : Data msg -> Html msg
element data =
    div
        [ class "two-column" ]
        [ div
            [ class "column-one" ]
            [ data.first ]
        , div
            [ class "column-two" ]
            [ data.second ]
        ]
