module Types.Domain exposing
    ( Domain(..)
    , decoder
    , encode
    , fromInt
    , list
    , option
    , toInt
    , toString
    )

import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


{-| Based on backend API definitions |
-}
type Domain
    = DomainNone
    | Foundation
    | Assessment
    | Investigation
    | Diagnosis
    | Management


toInt : Domain -> Int
toInt domain =
    case domain of
        DomainNone ->
            0

        Foundation ->
            1

        Assessment ->
            2

        Investigation ->
            3

        Diagnosis ->
            4

        Management ->
            5


fromInt : Int -> Domain
fromInt int =
    case int of
        1 ->
            Foundation

        2 ->
            Assessment

        3 ->
            Investigation

        4 ->
            Diagnosis

        5 ->
            Management

        _ ->
            DomainNone


toString : Domain -> String
toString domain =
    case domain of
        DomainNone ->
            "Unspecified"

        Foundation ->
            "Foundation Knowledge"

        Assessment ->
            "Assessment"

        Investigation ->
            "Investigation"

        Diagnosis ->
            "Diagnosis"

        Management ->
            "Manaagement"


list : List Domain
list =
    [ DomainNone
    , Foundation
    , Assessment
    , Investigation
    , Diagnosis
    , Management
    ]


option : Domain -> Html msg
option domain =
    Html.option
        [ Attributes.value (domain |> toInt |> String.fromInt) ]
        [ Html.text (domain |> toString) ]


encode : Domain -> Value
encode domain =
    toInt domain
        |> Encode.int


decoder : Decoder Domain
decoder =
    Decode.int
        |> Decode.map fromInt
