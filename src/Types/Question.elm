module Types.Question exposing (..)

{-| A multiple-choice question - the core question-bank feature of AORTA.

Each question has a parent objective, which is tagged with a specialty, topic
and medical training stage.

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Datetime as Datetime
import Types.Objective as Objective
import Types.Specialty as Specialty exposing (Specialty(..))
import Types.Stage as Stage exposing (Stage(..))
import Types.Topic as Topic exposing (Topic(..))
import Types.User as User


{-| Basic question data without the choices.

This is used for viewing questions on the parent objective page.

-}
type alias GetBasicData =
    { id : Int
    , objectiveId : Int
    , contributor : User.GetData
    , createdAt : Posix
    , modifiedAt : Posix
    , stem : String
    }


type alias GetDetailData =
    { id : Int
    , objective : Objective.GetData
    , contributor : User.GetData
    , createdAt : Posix
    , modifiedAt : Posix
    , stem : String
    , choices : Dict Int Choice.GetData
    , comments : List Comment.GetData
    , averageRating : Float
    }


{-| Decode basic question data without the choices.
-}
decoderBasic : Decoder GetBasicData
decoderBasic =
    Decode.succeed GetBasicData
        |> required "id" Decode.int
        |> required "objective" Decode.int
        |> required "contributor" User.decoder
        |> required "created_at" Datetime.decoder
        |> required "modified_at" Datetime.decoder
        |> required "stem" Decode.string


decoderDetail : Decoder GetDetailData
decoderDetail =
    Decode.succeed GetDetailData
        |> required "id" Decode.int
        |> required "objective" Objective.decoder
        |> required "contributor" User.decoder
        |> required "created_at" Datetime.decoder
        |> required "modified_at" Datetime.decoder
        |> required "stem" Decode.string
        |> required "choices" Choice.decodeDictionary
        |> required "comments" (Decode.list Comment.decoder)
        |> required "average_rating" Decode.float


{-| Data required to add a question.
-}
type alias PostData =
    { stem : String
    , choices : Dict Int Choice.PostData
    , objectiveId : Int
    }


{-| Encode a question for creation.
-}
encode : PostData -> Value
encode data =
    Encode.object
        [ ( "objective_id", Encode.int data.objectiveId )
        , ( "stem", Encode.string data.stem )
        , ( "choices", Encode.list Choice.encode (Dict.values data.choices) )
        ]


{-| Initialise a new default question.
-}
init : Int -> PostData
init objectiveId =
    { stem = ""
    , choices =
        Dict.fromList
            [ ( 0, Choice.newCorrect )
            , ( 1, Choice.newIncorrect )
            ]
    , objectiveId = objectiveId
    }


updateStem : String -> PostData -> PostData
updateStem string data =
    { data | stem = string }


updateChoiceContent : Int -> String -> PostData -> PostData
updateChoiceContent key string data =
    { data | choices = Dict.update key (Maybe.map <| Choice.withContent string) data.choices }


updateChoiceExplanation : Int -> String -> PostData -> PostData
updateChoiceExplanation key string data =
    { data | choices = Dict.update key (Maybe.map <| Choice.withExplanation string) data.choices }


{-| Adds a new distractor (incorrect choice) to the list of choices.
-}
addChoice : Int -> PostData -> PostData
addChoice key data =
    { data | choices = Dict.insert key Choice.newIncorrect data.choices }


{-| Removes a choice from the list of choices.
-}
removeChoice : Int -> PostData -> PostData
removeChoice key data =
    { data | choices = Dict.remove key data.choices }
