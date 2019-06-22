module Types.Choice exposing
    ( Choice
    , decoder
    , encode
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Choice =
    { content : String
    , explanation : String
    , isCorrect : Bool
    }


encode : Choice -> Value
encode data =
    Encode.object
        [ ( "content", Encode.string data.content )
        , ( "explanation", Encode.string data.explanation )
        , ( "is_correct", Encode.bool data.isCorrect )
        ]


decoder : Decoder Choice
decoder =
    Decode.map3 Choice
        (Decode.field "content" Decode.string)
        (Decode.field "explanation" Decode.string)
        (Decode.field "is_correct" Decode.bool)
