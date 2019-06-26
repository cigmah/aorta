module Types.Comment exposing (CreationData, ReadData, decoder, encode)

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.User as User exposing (User)


type alias CreationData =
    { noteId : Int, content : String }


type alias ReadData =
    { author : User
    , content : String
    , created_at : Posix
    }



-- Json


encode : CreationData -> Value
encode data =
    Encode.object
        [ ( "note", Encode.int data.noteId )
        , ( "content", Encode.string data.content )
        ]


decoder : Decoder ReadData
decoder =
    Decode.succeed ReadData
        |> required "author" User.decoder
        |> required "content" Decode.string
        |> required "created_at" Iso8601.decoder
