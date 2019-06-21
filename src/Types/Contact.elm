module Types.Contact exposing (Data, encode, init, responseDecoder)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type alias Data =
    { name : String
    , email : String
    , subject : String
    , body : String
    , loading : Bool
    }


init : Data
init =
    { name = ""
    , email = ""
    , subject = ""
    , body = ""
    , loading = False
    }


encode : Data -> Value
encode data =
    Encode.object
        [ ( "name", Encode.string data.name )
        , ( "email", Encode.string data.email )
        , ( "subject", Encode.string data.subject )
        , ( "content", Encode.string data.body )
        ]


responseDecoder : Decoder Bool
responseDecoder =
    Decode.succeed True
