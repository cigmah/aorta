module Types.System exposing
    ( System(..)
    , enumerable
    , toIcon
    )

{-| Contains the System type and operations relating to the System type.

The System type represents a medical system, and is one of the
categories used to classify objectives.

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Svg exposing (Svg)
import Types.Icon as Icon
import Types.Interface exposing (Enumerable)


{-| Represents a single medical system.

These medical system types are enumerated both here and in the backend. In
the database, they are stored as integer codes. It's important to ensure that
the integer codes on the frontend and backend match.

-}
type System
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


{-| Implementation of the enumerable interface for system.
-}
enumerable : Enumerable System
enumerable =
    { list = list
    , count = count
    , toString = toString
    , toBriefString = toBriefString
    , toInt = toInt
    , fromInt = fromInt
    , encode = encode
    , decoder = decoder
    }


{-| Converts a system to an integer code.
-}
toInt : System -> Int
toInt system =
    case system of
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


{-| Converts an integer code to a system.

The default case is Principles.

-}
fromInt : Int -> System
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


{-| Converts a system to a full string.
-}
toString : System -> String
toString system =
    case system of
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


{-| Converts a system to a brief string.
-}
toBriefString : System -> String
toBriefString system =
    case system of
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


{-| A constant which enumerates the full list of systems.
-}
list : List System
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


{-| A constant of the full count of systems.
-}
count : Int
count =
    List.length list


{-| Encodes a system as JSON.
-}
encode : System -> Value
encode system =
    toInt system
        |> Encode.int


{-| A JSON decoder for a system.
-}
decoder : Decoder System
decoder =
    Decode.int
        |> Decode.map fromInt


{-| Converts a system to an SVG icon from Types.Icon
-}
toIcon : System -> Svg msg
toIcon system =
    case system of
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
