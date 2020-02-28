module Element.SearchBar exposing (..)

{-| A generic search bar.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { value : String
    , placeholder : String
    , onInput : String -> msg
    , onSearch : msg
    }


element : Data msg -> Html msg
element data =
    Html.form
        [ class "search-bar"
        , onSubmit data.onSearch
        ]
        [ input
            [ type_ "search"
            , placeholder data.placeholder
            , onInput data.onInput
            , class "search-bar-input"
            , value data.value
            ]
            []
        , button
            [ type_ "submit"
            , class "search-bar-button"
            ]
            [ text "Search" ]
        ]
