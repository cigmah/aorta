module Types.Topic exposing
    ( Topic(..)
    , enumerable
    )

{-| Contains the Topic type and operations relating to the Topic type.

The Topic type represents a medical topic (such as Anatomy), and is one of
the categories used to classify objectives.

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Types.Interface exposing (Enumerable)


{-| Represents a single medical topic.

These topics are enumerated in both the frontend and backend, and need to be
kept in sync.

-}
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
    | DisordersEmergency
    | DisordersSpecific
    | DisordersPaediatric
    | DisordersPrimaryCarePrevention
    | DisordersTraumaExternal
    | MiscellaneousTopics


{-| Implementation of the enumerable interface for topic.
-}
enumerable : Enumerable Topic
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


{-| Converts a topic to an integer code.
-}
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

        DisordersEmergency ->
            14

        DisordersSpecific ->
            15

        DisordersPaediatric ->
            16

        DisordersPrimaryCarePrevention ->
            17

        DisordersTraumaExternal ->
            18

        MiscellaneousTopics ->
            19


{-| Converts an integer code to a topic.

The default case is Overview.

-}
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
            DisordersEmergency

        15 ->
            DisordersSpecific

        16 ->
            DisordersPaediatric

        17 ->
            DisordersPrimaryCarePrevention

        18 ->
            DisordersTraumaExternal

        19 ->
            MiscellaneousTopics

        _ ->
            Overview


{-| Converts a topic to a full string.
-}
toString : Topic -> String
toString topic =
    case topic of
        Overview ->
            "Professional Practice"

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

        DisordersEmergency ->
            "Disorders - Emergency"

        DisordersSpecific ->
            "Disorders - Specific"

        DisordersPaediatric ->
            "Disorders - Paediatric"

        DisordersPrimaryCarePrevention ->
            "Disorders - Primary Care & Prevention"

        DisordersTraumaExternal ->
            "Disorders - Trauma & External Causes"

        MiscellaneousTopics ->
            "Miscellaneous Topics"


{-| Converts a topic to a brief string.
-}
toBriefString : Topic -> String
toBriefString topic =
    case topic of
        Overview ->
            "Profession"

        GlobalIssues ->
            "Epidemiology"

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

        DisordersEmergency ->
            "Emergencies"

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


{-| A list of all topics.
-}
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
    , DisordersEmergency
    , DisordersSpecific
    , DisordersPaediatric
    , DisordersPrimaryCarePrevention
    , DisordersTraumaExternal
    , MiscellaneousTopics
    ]


{-| The number of topics available.
-}
count : Int
count =
    List.length list


{-| Converts a topic to a JSON value
-}
encode : Topic -> Value
encode topic =
    toInt topic
        |> Encode.int


{-| A JSON Decoder for a topic.
-}
decoder : Decoder Topic
decoder =
    Decode.int
        |> Decode.map fromInt
