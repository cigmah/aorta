module Types.Question exposing
    ( CreationData
    , ReadData
    , decoder
    , encode
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Choice as Choice exposing (Choice)
import Types.Note as Note
import Types.User as User exposing (User)


type alias CreationData =
    { stem : String
    , choices : List Choice
    }


type alias ReadData =
    { id : Int
    , note : Note.ReadData
    , stem : String
    , choices : List Choice
    , createdAt : Posix
    , modifiedAt : Posix
    , contributor : User
    }



-- Json


encode : CreationData -> Value
encode data =
    Encode.object
        [ ( "stem", Encode.string data.stem )
        , ( "choices", Encode.list Choice.encode data.choices )
        ]


decoder : Decoder ReadData
decoder =
    Decode.succeed ReadData
        |> required "id" Decode.int
        |> required "note" Note.decoder
        |> required "stem" Decode.string
        |> required "choices" (Decode.list Choice.decoder)
        |> required "created_at" Iso8601.decoder
        |> required "modified_at" Iso8601.decoder
        |> required "contributor" User.decoder
