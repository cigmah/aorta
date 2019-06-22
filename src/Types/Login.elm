module Types.Login exposing (Data, encode, init, responseDecoder)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Types.Credentials as Credentials exposing (Credentials)


type alias Data =
    { username : String, password : String, loading : Bool }


init : Data
init =
    { username = "", password = "", loading = False }


encode : Data -> Value
encode v =
    Encode.object
        [ ( "username", Encode.string v.username )
        , ( "password", Encode.string v.password )
        ]


responseDecoder : Decoder Credentials
responseDecoder =
    Decode.map2 Credentials
        (Decode.field "token" Decode.string)
        (Decode.field "username" Decode.string)
