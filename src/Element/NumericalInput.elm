module Element.NumericalInput exposing (..)

{-| A generic numerical input element with a label.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { label : String -- label shown above input field
    , value : String -- value for the text input
    , min : Int
    , max : Int
    , onInput :
        String
        -> msg -- event emitted when input text is changed
    }


element : Data msg -> Html msg
element data =
    div
        [ class "numerical-input" ]
        [ div
            [ class "numerical-input-label" ]
            [ label
                [ for data.label
                , class "numerical-input-label-text"
                ]
                [ text data.label ]
            ]
        , input
            [ type_ "number"
            , class "numerical-input-input"
            , id data.label
            , value data.value
            , Html.Attributes.min (String.fromInt data.min)
            , Html.Attributes.max (String.fromInt data.max)
            , onInput data.onInput
            , required True
            ]
            []
        ]
