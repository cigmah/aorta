module Types.Comment exposing (GetData, PostData, decoder, encode)

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.User as User


type alias PostData =
    { noteId : Int
    , content : String
    }


type alias GetData =
    { contributor : Maybe User.GetData
    , content : String
    , createdAt : Posix
    }


encode : PostData -> Value
encode data =
    Encode.object
        [ ( "note", Encode.int data.noteId )
        , ( "content", Encode.string data.content )
        ]


decoder : Decoder GetData
decoder =
    Decode.succeed GetData
        |> required "contributor" (Decode.maybe User.decoder)
        |> required "content" Decode.string
        |> required "created_at" Iso8601.decoder
