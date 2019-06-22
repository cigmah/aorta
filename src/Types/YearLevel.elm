module Types.YearLevel exposing
    ( YearLevel(..)
    , decoder
    , encode
    , fromInt
    , toInt
    , toString
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


{-| Based on the backend API definitions. |
-}
type YearLevel
    = YearNone
    | Year1
    | Year2a
    | Year3b
    | Year4c
    | Year5d


toInt : YearLevel -> Int
toInt yearLevel =
    case yearLevel of
        YearNone ->
            0

        Year1 ->
            1

        Year2a ->
            2

        Year3b ->
            3

        Year4c ->
            4

        Year5d ->
            5


fromInt : Int -> YearLevel
fromInt int =
    case int of
        1 ->
            Year1

        2 ->
            Year2a

        3 ->
            Year3b

        4 ->
            Year4c

        5 ->
            Year5d

        _ ->
            YearNone


toString : YearLevel -> String
toString yearLevel =
    case yearLevel of
        YearNone ->
            "Unspecified"

        Year1 ->
            "Year 1"

        Year2a ->
            "Year 2A"

        Year3b ->
            "Year 3B"

        Year4c ->
            "Year 4C"

        Year5d ->
            "Year 5D"


list : List YearLevel
list =
    [ YearNone
    , Year1
    , Year2a
    , Year3b
    , Year4c
    , Year5d
    ]


encode : YearLevel -> Value
encode yearLevel =
    toInt yearLevel
        |> Encode.int


decoder : Decoder YearLevel
decoder =
    Decode.int
        |> Decode.map fromInt
