module Types.Specialty exposing
    ( Specialty(..)
    , count
    , decoder
    , encode
    , fromInt
    , list
    , toBriefString
    , toIcon
    , toInt
    , toString
    )

{-| Contains the Specialty type and operations relating to the Specialty type.

The Specialty type represents a medical specialty, and is one of the
categories used to classify objectives.

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Svg exposing (Svg)
import Types.Icon as Icon


{-| Represents a single medical specialty.

These medical specialty types are enumerated both here and in the backend. In
the database, they are stored as integer codes. It's important to ensure that
the integer codes on the frontend and backend match.

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


{-| Converts a specialty to an integer code.
-}
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


{-| Converts an integer code to a specialty.

The default case is Principles.

-}
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


{-| Converts a specialty to a full string.
-}
toString : Specialty -> String
toString specialty =
    case specialty of
        Principles ->
            "Principles"

        Cardiovascular ->
            "Cardiology and Cardiothoracic Surgery"

        Respiratory ->
            "Respiratory Medicine"

        Gastrointestinal ->
            "Gastroenterology"

        RenalAndUrological ->
            "Renal Medicine and Urology"

        MusculoskeletalAndRheumatological ->
            "Orthopedics and Rheumatology"

        Neurological ->
            "Neurology and Neurosurgery"

        Haematological ->
            "Haematology"

        Endocrine ->
            "Endocrinology"

        MentalAndBehavioural ->
            "Psychiatry and Psychology"

        ObstetricAndGynaecological ->
            "Obstetrics and Gynaecology"

        Otolaryngological ->
            "Ear, Nose and Throat"

        Ophthalmological ->
            "Ophthalmology"

        Dermatological ->
            "Dermatology"


{-| Converts a specialty to a brief string.
-}
toBriefString : Specialty -> String
toBriefString specialty =
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


{-| A constant which enumerates the full list of specialties.
-}
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


{-| A constant of the full count of specialties.
-}
count : Int
count =
    List.length list


{-| Encodes a specialty as JSON.
-}
encode : Specialty -> Value
encode specialty =
    toInt specialty
        |> Encode.int


{-| A JSON decoder for a specialty.
-}
decoder : Decoder Specialty
decoder =
    Decode.int
        |> Decode.map fromInt


{-| Converts a specialty to an SVG icon from Types.Icon
-}
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
