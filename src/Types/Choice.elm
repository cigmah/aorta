module Types.Choice exposing
    ( Category
    , Choice
    , ChoiceBase
    , decoder
    , encode
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode


{-| A category allocated to each choice, stored in the database as an integer enum
field. |
-}
type Category
    = Uncategorised
    | Symptom
    | Sign
    | Diagnosis
    | Investigation
    | Medication
    | Intervention
    | Pathogen
    | Vaccine


{-| As these are defined by the backend, I prefer to have this specified in
full rather than generated using List.indexedMap. |
-}
categoryToInt : Category -> Int
categoryToInt category =
    case category of
        Uncategorised ->
            0

        Symptom ->
            1

        Sign ->
            2

        Diagnosis ->
            3

        Investigation ->
            4

        Medication ->
            5

        Intervention ->
            6

        Pathogen ->
            7

        Vaccine ->
            8


intToCategory : Int -> Category
intToCategory int =
    case int of
        0 ->
            Uncategorised

        1 ->
            Symptom

        2 ->
            Sign

        3 ->
            Diagnosis

        4 ->
            Investigation

        5 ->
            Medication

        6 ->
            Intervention

        7 ->
            Pathogen

        8 ->
            Vaccine

        _ ->
            Uncategorised


{-| A choice object contains data relating to question choices. |
-}
type alias Choice =
    { id : Int
    , content : String
    , category : Category
    }


type alias ChoiceBase =
    { content : String
    , category : Category
    }



-- JSON


decoder : Decoder Choice
decoder =
    Decode.succeed Choice
        |> required "id" Decode.int
        |> required "content" Decode.string
        |> required "category" (Decode.map intToCategory Decode.int)


encode : ChoiceBase -> Value
encode choice =
    Encode.object
        [ ( "content", Encode.string choice.content )
        , ( "category", Encode.int (categoryToInt choice.category) )
        ]
