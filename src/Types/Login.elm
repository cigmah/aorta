module Types.Login exposing (PostData, decoder, encode, init)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Types.Credentials exposing (Credentials)


type alias PostData =
    { username : String
    , password : String
    }


init : PostData
init =
    { username = ""
    , password = ""
    }


encode : PostData -> Value
encode v =
    Encode.object
        [ ( "username", Encode.string v.username )
        , ( "password", Encode.string v.password )
        ]


decoder : Decoder Credentials
decoder =
    Decode.map2 Credentials
        (Decode.field "token" Decode.string)
        (Decode.field "username" Decode.string)
