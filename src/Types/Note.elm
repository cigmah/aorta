module Types.Note exposing
    ( ListData
    , decoderList
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Comment as Comment
import Types.Domain as Domain exposing (Domain)
import Types.Specialty as Specialty exposing (Specialty)
import Types.User as User exposing (User)
import Types.YearLevel as YearLevel exposing (YearLevel)


type alias ListData =
    { id : Int
    , yearLevel : YearLevel
    , specialty : Specialty
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
            |> required "year_level" YearLevel.decoder
            |> required "specialty" Specialty.decoder
            |> required "modified_at" Iso8601.decoder
            |> required "title" Decode.string
            |> required "num_questions" Decode.int
            |> required "num_comments" Decode.int
            |> optional "num_due" (Decode.maybe Decode.int) Nothing
            |> optional "num_known" (Decode.maybe Decode.int) Nothing
        )
