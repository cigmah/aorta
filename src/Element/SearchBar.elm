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
    div
        [ class "search-bar" ]
        [ input
            [ type_ "search"
            , placeholder data.placeholder
            , onInput data.onInput
            , class "search-bar-input"
            ]
            []
        , button
            [ type_ "button"
            , class "search-bar-button"
            , onClick data.onSearch
            ]
            [ text "Search" ]
        ]
