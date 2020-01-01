module Types.Stage exposing
    ( Stage(..)
    , enumerable
    )

{-| Contains the Stage type and operations relating to the Stage type.

The Stage type represents a stage of medical training, and is one of the
categories used to classify learning objectives.

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Types.Interface exposing (Enumerable)


{-| Represents a single medical training stage.

These medical stage types are enumerated both here and in the backend. In
the database, they are stored as integer codes. It's important to ensure that
the integer codes on the frontend and backend match.

-}
type Stage
    = Year1
    | Year2A
    | Year3B
    | Year4C
    | Year5D
    | Intern
    | Resident
    | Registrar


{-| Implementation of the enumerable interface for stage.
-}
enumerable : Enumerable Stage
enumerable =
    { list = list
    , count = count
    , toString = toString
    , toBriefString = toString
    , toInt = toInt
    , fromInt = fromInt
    , encode = encode
    , decoder = decoder
    }


{-| Converts a specialty to an integer code.
-}
toInt : Stage -> Int
toInt stage =
    case stage of
        Year1 ->
            0

        Year2A ->
            1

        Year3B ->
            2

        Year4C ->
            3

        Year5D ->
            4

        Intern ->
            5

        Resident ->
            6

        Registrar ->
            7


{-| Converts an integer code to a stage.

The default case is Year1.

-}
fromInt : Int -> Stage
fromInt int =
    case int of
        0 ->
            Year1

        1 ->
            Year2A

        2 ->
            Year3B

        3 ->
            Year4C

        4 ->
            Year5D

        5 ->
            Intern

        6 ->
            Resident

        7 ->
            Registrar

        _ ->
            Year1


{-| Converts a stage to a full string.
-}
toString : Stage -> String
toString stage =
    case stage of
        Year1 ->
            "Year 1"

        Year2A ->
            "Year 2A"

        Year3B ->
            "Year 3B"

        Year4C ->
            "Year 4C"

        Year5D ->
            "Year 5D"

        Intern ->
            "Intern"

        Resident ->
            "Resident"

        Registrar ->
            "Registrar"


{-| A constant which enumerates the list of stages.

Intern, Resident and Registrar are excluded for now, as we should not expect
questions for these stages yet.

-}
list : List Stage
list =
    [ Year1
    , Year2A
    , Year3B
    , Year4C
    , Year5D
    ]


{-| A constant of the full count of stages.
-}
count : Int
count =
    List.length list


{-| Encodes a stage as JSON.
-}
encode : Stage -> Value
encode stage =
    toInt stage
        |> Encode.int


{-| A JSON decoder for a specialty.
-}
decoder : Decoder Stage
decoder =
    Decode.int
        |> Decode.map fromInt
