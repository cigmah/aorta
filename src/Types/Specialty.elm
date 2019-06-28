module Types.Specialty exposing
    ( Specialty(..)
    , count
    , decoder
    , encode
    , fromInt
    , list
    , option
    , toColor
    , toInt
    , toString
    )

import Color
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


{-| Based on backend API definitions. It's verbose, but was easy to script,
so we didn't have to write this manually. |
-}
type Specialty
    = SpecialtyNone
    | Anatomy
    | Physiology
    | Biochemistry
    | Genetics
    | Pharmacology
    | Psychology
    | Microbiology
    | SocialIssues
    | PopulationHealth
    | ClinicalSkills
    | Cardiovascular
    | Respiratory
    | Gastrointestinal
    | Hepatobiliary
    | Genitourinary
    | Endocrine
    | Neurological
    | Musculoskeletal
    | Haematological
    | InfectiousDisease
    | Dermatological
    | Pathology
    | GeneralPractice
    | Psychiatry
    | Obgyn
    | Paediatrics
    | Law
    | Ethics


toInt : Specialty -> Int
toInt specialty =
    case specialty of
        SpecialtyNone ->
            0

        Anatomy ->
            1

        Physiology ->
            2

        Biochemistry ->
            3

        Genetics ->
            4

        Pharmacology ->
            5

        Psychology ->
            6

        Microbiology ->
            7

        SocialIssues ->
            8

        PopulationHealth ->
            9

        ClinicalSkills ->
            10

        Cardiovascular ->
            11

        Respiratory ->
            12

        Gastrointestinal ->
            13

        Hepatobiliary ->
            14

        Genitourinary ->
            15

        Endocrine ->
            16

        Neurological ->
            17

        Musculoskeletal ->
            18

        Haematological ->
            19

        InfectiousDisease ->
            20

        Dermatological ->
            21

        Pathology ->
            22

        GeneralPractice ->
            23

        Psychiatry ->
            24

        Obgyn ->
            25

        Paediatrics ->
            26

        Law ->
            27

        Ethics ->
            28


fromInt : Int -> Specialty
fromInt int =
    case int of
        1 ->
            Anatomy

        2 ->
            Physiology

        3 ->
            Biochemistry

        4 ->
            Genetics

        5 ->
            Pharmacology

        6 ->
            Psychology

        7 ->
            Microbiology

        8 ->
            SocialIssues

        9 ->
            PopulationHealth

        10 ->
            ClinicalSkills

        11 ->
            Cardiovascular

        12 ->
            Respiratory

        13 ->
            Gastrointestinal

        14 ->
            Hepatobiliary

        15 ->
            Genitourinary

        16 ->
            Endocrine

        17 ->
            Neurological

        18 ->
            Musculoskeletal

        19 ->
            Haematological

        20 ->
            InfectiousDisease

        21 ->
            Dermatological

        22 ->
            Pathology

        23 ->
            GeneralPractice

        24 ->
            Psychiatry

        25 ->
            Obgyn

        26 ->
            Paediatrics

        27 ->
            Law

        28 ->
            Ethics

        _ ->
            SpecialtyNone


toString : Specialty -> String
toString specialty =
    case specialty of
        SpecialtyNone ->
            "Unspecified"

        Anatomy ->
            "Anatomy"

        Physiology ->
            "Physiology"

        Biochemistry ->
            "Biochemistry"

        Genetics ->
            "Genetics"

        Pharmacology ->
            "Pharmacology"

        Psychology ->
            "Psychology"

        Microbiology ->
            "Microbiology"

        SocialIssues ->
            "Social Issues"

        PopulationHealth ->
            "Population Health"

        ClinicalSkills ->
            "Clinical Skills"

        Cardiovascular ->
            "Cardiovascular"

        Respiratory ->
            "Respiratory"

        Gastrointestinal ->
            "Gastrointestinal"

        Hepatobiliary ->
            "Hepatobiliary"

        Genitourinary ->
            "Genitourinary"

        Endocrine ->
            "Endocrine"

        Neurological ->
            "Neurological"

        Musculoskeletal ->
            "Musculoskeletal"

        Haematological ->
            "Haematological"

        InfectiousDisease ->
            "Infectious Disease"

        Dermatological ->
            "Dermatological"

        Pathology ->
            "Pathology"

        GeneralPractice ->
            "General Practice"

        Psychiatry ->
            "Psychiatry"

        Obgyn ->
            "Women's Health"

        Paediatrics ->
            "Children's Health"

        Law ->
            "Law"

        Ethics ->
            "Ethics"


list : List Specialty
list =
    [ SpecialtyNone
    , Anatomy
    , Physiology
    , Biochemistry
    , Genetics
    , Pharmacology
    , Psychology
    , Microbiology
    , SocialIssues
    , PopulationHealth
    , ClinicalSkills
    , Cardiovascular
    , Respiratory
    , Gastrointestinal
    , Hepatobiliary
    , Genitourinary
    , Endocrine
    , Neurological
    , Musculoskeletal
    , Haematological
    , InfectiousDisease
    , Dermatological
    , Pathology
    , GeneralPractice
    , Psychiatry
    , Obgyn
    , Paediatrics
    , Law
    , Ethics
    ]


count : Int
count =
    List.length list


toColor : Specialty -> Color.Color
toColor specialty =
    Color.hsla
        (toFloat (modBy count (toInt specialty * 3)) / (toFloat count - 1))
        0.4
        0.7
        1


option : Specialty -> Specialty -> Html msg
option selected specialty =
    Html.option
        [ Attributes.value (specialty |> toInt |> String.fromInt)
        , Attributes.selected (selected == specialty)
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
