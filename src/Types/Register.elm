module Types.Register exposing (PostData, Response, encode, init, decoder)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type alias PostData =
    { username : String, email : String, loading : Bool }


init : PostData
init =
    { username = "", email = "", loading = False }


encode : PostData -> Value
encode v =
    Encode.object
        [ ( "username", Encode.string v.username )
        , ( "email", Encode.string v.email )
        ]


type alias Response =
    { username : String, token : String, password : String }


decoder : Decoder Response
decoder =
    Decode.map3 Response
        (Decode.field "username" Decode.string)
        (Decode.field "token" Decode.string)
        (Decode.field "password" Decode.string)
