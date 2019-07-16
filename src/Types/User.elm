module Types.User exposing (Stats, User, anonymous, decoder)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias User =
    { id : Int, username : String }


decoder : Decoder User
decoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)


type alias Stats =
    { answer : Int }


anonymous : User
anonymous =
    { id = -1, username = "Anonymous" }
