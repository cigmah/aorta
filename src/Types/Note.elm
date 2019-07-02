module Types.Note exposing
    ( Data
    , ListData
    , decoder
    , decoderList
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Comment as Comment
import Types.Domain as Domain exposing (Domain)
import Types.Question as Question
import Types.Specialty as Specialty exposing (Specialty)
import Types.Topic as Topic exposing (Topic)
import Types.User as User exposing (User)
import Types.YearLevel as YearLevel exposing (YearLevel)


type alias ListData =
    { id : Int
    , specialty : Specialty
    , topic : Topic
    , modifiedAt : Posix
    , title : String
    , numQuestions : Int
    , numComments : Int
    , numDue : Maybe Int
    , numKnown : Maybe Int
    }


decoderList : Decoder (List ListData)
decoderList =
    Decode.list
        (Decode.succeed ListData
            |> required "id" Decode.int
            |> required "specialty" Specialty.decoder
            |> required "topic" Topic.decoder
            |> required "modified_at" Iso8601.decoder
            |> required "title" Decode.string
            |> required "num_questions" Decode.int
            |> required "num_comments" Decode.int
            |> optional "num_due" (Decode.maybe Decode.int) Nothing
            |> optional "num_known" (Decode.maybe Decode.int) Nothing
        )


type alias Data =
    { id : Int
    , specialty : Specialty
    , topic : Topic
    , title : String
    , content : String
    , contributor : Maybe User
    , modifiedAt : Posix
    , comments : List Comment.ReadData
    , allIds : List Question.ListData
    , dueIds : Maybe (List Question.ListData)
    , knownIds : Maybe (List Question.ListData)
    }


decoder : Decoder Data
decoder =
    Decode.succeed Data
        |> required "id" Decode.int
        |> required "specialty" Specialty.decoder
        |> required "topic" Topic.decoder
        |> required "title" Decode.string
        |> required "content" Decode.string
        |> required "contributor" (Decode.maybe User.decoder)
        |> required "modified_at" Iso8601.decoder
        |> required "comments" (Decode.list Comment.decoder)
        |> required "all_ids" (Decode.list Question.decoderList)
        |> optional "due_ids" (Decode.map Just (Decode.list Question.decoderList)) Nothing
        |> optional "known_ids" (Decode.map Just (Decode.list Question.decoderList)) Nothing
