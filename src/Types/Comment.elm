module Types.Comment exposing (Comment, decoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.User as User exposing (User)


type alias Comment =
    { user : User
    , content : String
    , timestamp : Posix
    }



-- Json


decoder : Decoder Comment
decoder =
    Decode.succeed Comment
        |> required "user" User.decoder
        |> required "content" Decode.string
        |> required "timestamp" Iso8601.decoder
