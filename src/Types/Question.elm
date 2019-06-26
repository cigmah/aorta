module Types.Question exposing
    ( CreationData
    , ReadData
    , decoder
    , encode
    , new
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Choice as Choice
import Types.Note as Note
import Types.User as User exposing (User)


type alias CreationData =
    { stem : String
    , choices : List Choice.CreationData
    }


type alias ReadData =
    { id : Int
    , stem : String
    , choices : List Choice.ReadData
    , createdAt : Posix
    , modifiedAt : Posix
    , contributor : User
    }


new : CreationData
new =
    { stem = ""
    , choices = [ Choice.newCorrect, Choice.newIncorrect ]
    }



-- Json


encode : Int -> CreationData -> Value
encode noteId data =
    Encode.object
        [ ( "note_id", Encode.int noteId )
        , ( "stem", Encode.string data.stem )
        , ( "choices", Encode.list Choice.encode data.choices )
        ]


decoder : Decoder ReadData
decoder =
    Decode.succeed ReadData
        |> required "id" Decode.int
        |> required "stem" Decode.string
        |> required "choices" (Decode.list Choice.decoder)
        |> required "created_at" Iso8601.decoder
        |> required "modified_at" Iso8601.decoder
        |> required "contributor" User.decoder
