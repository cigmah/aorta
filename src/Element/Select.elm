module Element.Select exposing (..)

{-| A single select for an enumerable type.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Interface exposing (Enumerable)


type alias Data a msg =
    { value : a
    , label : String
    , enumerable : Enumerable a
    , onInput : String -> msg
    }


option : Enumerable a -> a -> Html msg
option enumerable item =
    Html.option
        [ value (item |> enumerable.toInt |> String.fromInt) ]
        [ text (enumerable.toBriefString item) ]


element : Data a msg -> Html msg
element data =
    div
        [ class "select-group" ]
        [ label
            [ class "select-label" ]
            [ text data.label ]
        , select
            [ class "select-select"
            , value (data.value |> data.enumerable.toInt |> String.fromInt)
            , onInput data.onInput
            ]
            (List.map (option data.enumerable) data.enumerable.list)
        ]
