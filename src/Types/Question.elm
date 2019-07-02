module Types.Question exposing
    ( CreationData
    , ListData
    , ReadData
    , decoder
    , decoderList
    , encode
    , new
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Domain as Domain exposing (Domain)
import Types.User as User exposing (User)
import Types.YearLevel as YearLevel exposing (YearLevel)


type alias CreationData =
    { stem : String
    , domain : Domain
    , yearLevel : YearLevel
    , choices : List Choice.CreationData
    }


type alias ReadData =
    { id : Int
    , stem : String
    , domain : Domain
    , yearLevel : YearLevel
    , choices : List Choice.ReadData
    , contributor : User
    , comments : List Comment.ReadData
    , numLikes : Maybe Int
    , liked : Maybe Bool
    , numSeen : Maybe Int
    , modifiedAt : Posix
    }


type alias ListData =
    { id : Int }


new : CreationData
new =
    { stem = ""
    , domain = Domain.DomainNone
    , yearLevel = YearLevel.YearNone
    , choices = [ Choice.newCorrect, Choice.newIncorrect ]
    }



-- Json


encode : Int -> CreationData -> Value
encode noteId data =
    Encode.object
        [ ( "note_id", Encode.int noteId )
        , ( "stem", Encode.string data.stem )
        , ( "domain", Domain.encode data.domain )
        , ( "year_level", YearLevel.encode data.yearLevel )
        , ( "choices", Encode.list Choice.encode data.choices )
        ]


decoder : Decoder ReadData
decoder =
    Decode.succeed ReadData
        |> required "id" Decode.int
        |> required "stem" Decode.string
        |> required "domain" Domain.decoder
        |> required "year_level" YearLevel.decoder
        |> required "choices" (Decode.list Choice.decoder)
        |> required "contributor" User.decoder
        |> required "comments" (Decode.list Comment.decoder)
        |> required "num_likes" (Decode.maybe Decode.int)
        |> required "liked" (Decode.maybe Decode.bool)
        |> required "num_seen" (Decode.maybe Decode.int)
        |> required "modified_at" Iso8601.decoder


decoderList : Decoder ListData
decoderList =
    Decode.map ListData
        (Decode.field "id" Decode.int)
