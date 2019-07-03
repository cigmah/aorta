module Types.Topic exposing
    ( Topic(..)
    , count
    , decoder
    , encode
    , fromInt
    , list
    , option
    , toBrief
    , toInt
    , toString
    )

import Color
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type Topic
    = Overview
    | GlobalIssues
    | Development
    | CellLevelStructure
    | OrganLevelStructure
    | TheoryOfNormalFunction
    | TheoryOfAbnormalFunction
    | Medications
    | ClinicalHistory
    | ClinicalExam
    | ClinicalInvestigtions
    | ClinicalProcedures
    | DisordersInfectious
    | DisordersNeoplastic
    | DisordersSpecific
    | DisordersPaediatric
    | DisordersPrimaryCarePrevention
    | DisordersTraumaExternal
    | MiscellaneousTopics


toInt : Topic -> Int
toInt topic =
    case topic of
        Overview ->
            0

        GlobalIssues ->
            1

        Development ->
            2

        CellLevelStructure ->
            3

        OrganLevelStructure ->
            4

        TheoryOfNormalFunction ->
            5

        TheoryOfAbnormalFunction ->
            6

        Medications ->
            7

        ClinicalHistory ->
            8

        ClinicalExam ->
            9

        ClinicalInvestigtions ->
            10

        ClinicalProcedures ->
            11

        DisordersInfectious ->
            12

        DisordersNeoplastic ->
            13

        DisordersSpecific ->
            14

        DisordersPaediatric ->
            15

        DisordersPrimaryCarePrevention ->
            16

        DisordersTraumaExternal ->
            17

        MiscellaneousTopics ->
            18


fromInt : Int -> Topic
fromInt int =
    case int of
        0 ->
            Overview

        1 ->
            GlobalIssues

        2 ->
            Development

        3 ->
            CellLevelStructure

        4 ->
            OrganLevelStructure

        5 ->
            TheoryOfNormalFunction

        6 ->
            TheoryOfAbnormalFunction

        7 ->
            Medications

        8 ->
            ClinicalHistory

        9 ->
            ClinicalExam

        10 ->
            ClinicalInvestigtions

        11 ->
            ClinicalProcedures

        12 ->
            DisordersInfectious

        13 ->
            DisordersNeoplastic

        14 ->
            DisordersSpecific

        15 ->
            DisordersPaediatric

        16 ->
            DisordersPrimaryCarePrevention

        17 ->
            DisordersTraumaExternal

        18 ->
            MiscellaneousTopics

        _ ->
            Overview


toString : Topic -> String
toString topic =
    case topic of
        Overview ->
            "Overview"

        GlobalIssues ->
            "Global Issues"

        Development ->
            "Development"

        CellLevelStructure ->
            "Cell Level Structure"

        OrganLevelStructure ->
            "Organ Level Structure"

        TheoryOfNormalFunction ->
            "Theory Of Normal Function"

        TheoryOfAbnormalFunction ->
            "Theory Of Abnormal Function"

        Medications ->
            "Medications"

        ClinicalHistory ->
            "Clinical History"

        ClinicalExam ->
            "Clinical Exam"

        ClinicalInvestigtions ->
            "Clinical Investigtions"

        ClinicalProcedures ->
            "Clinical Procedures"

        DisordersInfectious ->
            "Disorders - Infectious"

        DisordersNeoplastic ->
            "Disorders - Neoplastic"

        DisordersSpecific ->
            "Disorders - Specific"

        DisordersPaediatric ->
            "Disorders - Paediatric"

        DisordersPrimaryCarePrevention ->
            "Disorders - Primary Care & Prevention"

        DisordersTraumaExternal ->
            "Disorders - Trauma External"

        MiscellaneousTopics ->
            "Miscellaneous Topics"


toBrief : Topic -> String
toBrief topic =
    case topic of
        Overview ->
            "Overview"

        GlobalIssues ->
            "Society"

        Development ->
            "Development"

        CellLevelStructure ->
            "Cells"

        OrganLevelStructure ->
            "Anatomy"

        TheoryOfNormalFunction ->
            "Physiology"

        TheoryOfAbnormalFunction ->
            "Pathology"

        Medications ->
            "Drugs"

        ClinicalHistory ->
            "History"

        ClinicalExam ->
            "Exam"

        ClinicalInvestigtions ->
            "Investigations"

        ClinicalProcedures ->
            "Procedures"

        DisordersInfectious ->
            "Infections"

        DisordersNeoplastic ->
            "Neoplasms"

        DisordersSpecific ->
            "Disorders"

        DisordersPaediatric ->
            "Paediatrics"

        DisordersPrimaryCarePrevention ->
            "GP"

        DisordersTraumaExternal ->
            "Trauma"

        MiscellaneousTopics ->
            "Misc."


list : List Topic
list =
    [ Overview
    , GlobalIssues
    , Development
    , CellLevelStructure
    , OrganLevelStructure
    , TheoryOfNormalFunction
    , TheoryOfAbnormalFunction
    , Medications
    , ClinicalHistory
    , ClinicalExam
    , ClinicalInvestigtions
    , ClinicalProcedures
    , DisordersInfectious
    , DisordersNeoplastic
    , DisordersSpecific
    , DisordersPaediatric
    , DisordersPrimaryCarePrevention
    , DisordersTraumaExternal
    , MiscellaneousTopics
    ]


count : Int
count =
    List.length list


option : Maybe Topic -> Topic -> Html msg
option selectedMaybe topic =
    let
        selected =
            case selectedMaybe of
                Just selectedValue ->
                    selectedValue == topic

                Nothing ->
                    False
    in
    Html.option
        [ Attributes.value (topic |> toInt |> String.fromInt)
        , Attributes.selected selected
        ]
        [ Html.text (topic |> toString) ]


encode : Topic -> Value
encode topic =
    toInt topic
        |> Encode.int


decoder : Decoder Topic
decoder =
    Decode.int
        |> Decode.map fromInt
