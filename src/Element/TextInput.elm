module Element.TextInput exposing (..)

{-| A generic text input element with a label.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { label : String -- label shown above input field
    , id : String -- text input id
    , information : String -- information next to the label on tooltip
    , inputType : String -- text input type
    , required : Bool -- whether the field is required, for form fields
    , placeholder : String -- placeholder text for input
    , value : String -- value for the text input
    , onInput :
        String
        -> msg -- event emitted when input text is changed
    }


element : Data msg -> Html msg
element data =
    div
        [ class "text-input" ]
        [ div
            [ class "text-input-label" ]
            [ label
                [ for data.id
                , class "text-input-label-text"
                ]
                [ text data.label ]
            , label
                [ for data.id
                , class "text-input-label-information"
                , attribute "data-tooltip" data.information
                ]
                [ text "🛈" ]
            ]
        , input
            [ type_ data.inputType
            , class "text-input-input"
            , id data.id
            , value data.value
            , placeholder data.placeholder
            , onInput data.onInput
            , required data.required
            ]
            []
        ]
