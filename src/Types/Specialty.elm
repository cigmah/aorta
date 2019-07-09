module Types.Specialty exposing
    ( Specialty(..)
    , count
    , decoder
    , encode
    , fromInt
    , list
    , option
    , toIcon
    , toInt
    , toString
    )

import Color
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Svg exposing (Svg)
import Types.Icon as Icon


{-| Based on backend API definitions. It's verbose, but was easy to script,
so we didn't have to write this manually. |
-}
type Specialty
    = Principles
    | Cardiovascular
    | Respiratory
    | Gastrointestinal
    | RenalAndUrological
    | MusculoskeletalAndRheumatological
    | Neurological
    | Haematological
    | Endocrine
    | MentalAndBehavioural
    | ObstetricAndGynaecological
    | Otolaryngological
    | Ophthalmological
    | Dermatological


toInt : Specialty -> Int
toInt specialty =
    case specialty of
        Principles ->
            0

        Cardiovascular ->
            1

        Respiratory ->
            2

        Gastrointestinal ->
            3

        RenalAndUrological ->
            4

        MusculoskeletalAndRheumatological ->
            5

        Neurological ->
            6

        Haematological ->
            7

        Endocrine ->
            8

        MentalAndBehavioural ->
            9

        ObstetricAndGynaecological ->
            10

        Otolaryngological ->
            11

        Ophthalmological ->
            12

        Dermatological ->
            13


fromInt : Int -> Specialty
fromInt int =
    case int of
        0 ->
            Principles

        1 ->
            Cardiovascular

        2 ->
            Respiratory

        3 ->
            Gastrointestinal

        4 ->
            RenalAndUrological

        5 ->
            MusculoskeletalAndRheumatological

        6 ->
            Neurological

        7 ->
            Haematological

        8 ->
            Endocrine

        9 ->
            MentalAndBehavioural

        10 ->
            ObstetricAndGynaecological

        11 ->
            Otolaryngological

        12 ->
            Ophthalmological

        13 ->
            Dermatological

        _ ->
            Principles


toString : Specialty -> String
toString specialty =
    case specialty of
        Principles ->
            "Principles"

        Cardiovascular ->
            "Cardio"

        Respiratory ->
            "Resp"

        Gastrointestinal ->
            "Gastro"

        RenalAndUrological ->
            "Renal"

        MusculoskeletalAndRheumatological ->
            "MSK"

        Neurological ->
            "Neuro"

        Haematological ->
            "Haem"

        Endocrine ->
            "Endo"

        MentalAndBehavioural ->
            "Psych"

        ObstetricAndGynaecological ->
            "Obgyn"

        Otolaryngological ->
            "ENT"

        Ophthalmological ->
            "Ophthal"

        Dermatological ->
            "Derm"


list : List Specialty
list =
    [ Principles
    , Cardiovascular
    , Respiratory
    , Gastrointestinal
    , RenalAndUrological
    , MusculoskeletalAndRheumatological
    , Neurological
    , Haematological
    , Endocrine
    , MentalAndBehavioural
    , ObstetricAndGynaecological
    , Otolaryngological
    , Ophthalmological
    , Dermatological
    ]


count : Int
count =
    List.length list


option : Maybe Specialty -> Specialty -> Html msg
option selectedMaybe specialty =
    let
        selected =
            case selectedMaybe of
                Just selectedValue ->
                    selectedValue == specialty

                Nothing ->
                    False
    in
    Html.option
        [ Attributes.value (specialty |> toInt |> String.fromInt)
        , Attributes.selected selected
        ]
        [ Html.text (specialty |> toString) ]


encode : Specialty -> Value
encode specialty =
    toInt specialty
        |> Encode.int


decoder : Decoder Specialty
decoder =
    Decode.int
        |> Decode.map fromInt


toIcon : Specialty -> Svg msg
toIcon specialty =
    case specialty of
        Principles ->
            Icon.principles

        Cardiovascular ->
            Icon.cardio

        Respiratory ->
            Icon.resp

        Gastrointestinal ->
            Icon.gastro

        RenalAndUrological ->
            Icon.renal

        MusculoskeletalAndRheumatological ->
            Icon.msk

        Neurological ->
            Icon.neuro

        Haematological ->
            Icon.haem

        Endocrine ->
            Icon.endo

        MentalAndBehavioural ->
            Icon.psych

        ObstetricAndGynaecological ->
            Icon.obgyn

        Otolaryngological ->
            Icon.ent

        Ophthalmological ->
            Icon.ophthal

        Dermatological ->
            Icon.derm
