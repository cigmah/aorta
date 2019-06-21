module Types.Register exposing (Data, Response, encode, init, responseDecoder)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type alias Data =
    { username : String, email : String, loading : Bool }


init : Data
init =
    { username = "", email = "", loading = False }


encode : Data -> Value
encode v =
    Encode.object
        [ ( "username", Encode.string v.username )
        , ( "email", Encode.string v.email )
        ]


type alias Response =
    { username : String, token : String, password : String }


responseDecoder : Decoder Response
responseDecoder =
    Decode.map3 Response
        (Decode.field "username" Decode.string)
        (Decode.field "token" Decode.string)
        (Decode.field "password" Decode.string)
