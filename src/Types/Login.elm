module Types.Login exposing (Data, Response, encode, init, responseDecoder)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


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


type alias Response =
    { token : String }


responseDecoder : Decoder Response
responseDecoder =
    Decode.map Response
        (Decode.field "token" Decode.string)
