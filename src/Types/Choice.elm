module Types.Choice exposing
    ( CreationData
    , ReadData
    , decoder
    , encode
    , newCorrect
    , newIncorrect
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias CreationData =
    { content : String
    , explanation : String
    , isCorrect : Bool
    }


type alias ReadData =
    { id : Int
    , content : String
    , explanation : String
    , isCorrect : Bool
    }


newCorrect : CreationData
newCorrect =
    { content = ""
    , explanation = ""
    , isCorrect = True
    }


newIncorrect : CreationData
newIncorrect =
    { content = ""
    , explanation = ""
    , isCorrect = False
    }


encode : CreationData -> Value
encode data =
    Encode.object
        [ ( "content", Encode.string data.content )
        , ( "explanation", Encode.string data.explanation )
        , ( "is_correct", Encode.bool data.isCorrect )
        ]


decoder : Decoder ReadData
decoder =
    Decode.map4 ReadData
        (Decode.field "id" Decode.int)
        (Decode.field "content" Decode.string)
        (Decode.field "explanation" Decode.string)
        (Decode.field "is_correct" Decode.bool)
