module Types.Choice exposing
    ( GetData
    , PostData
    , decoder
    , encode
    , newCorrect
    , newIncorrect
    , withContent
    , withExplanation
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type alias PostData =
    { content : String
    , explanation : String
    , isCorrect : Bool
    }


type alias GetData =
    { id : Int
    , content : String
    , explanation : String
    , isCorrect : Bool
    , numChosen : Int
    }


newCorrect : PostData
newCorrect =
    { content = ""
    , explanation = ""
    , isCorrect = True
    }


newIncorrect : PostData
newIncorrect =
    { content = ""
    , explanation = ""
    , isCorrect = False
    }


withContent : String -> PostData -> PostData
withContent content choice =
    { choice | content = content }


withExplanation : String -> PostData -> PostData
withExplanation explanation choice =
    { choice | explanation = explanation }


encode : PostData -> Value
encode data =
    Encode.object
        [ ( "content", Encode.string data.content )
        , ( "explanation", Encode.string data.explanation )
        , ( "is_correct", Encode.bool data.isCorrect )
        ]


decoder : Decoder GetData
decoder =
    Decode.map5 GetData
        (Decode.field "id" Decode.int)
        (Decode.field "content" Decode.string)
        (Decode.field "explanation" Decode.string)
        (Decode.field "is_correct" Decode.bool)
        (Decode.field "num_chosen" Decode.int)
