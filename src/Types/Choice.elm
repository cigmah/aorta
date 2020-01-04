module Types.Choice exposing
    ( GetData
    , PostData
    , decodeDictionary
    , decoder
    , encode
    , newCorrect
    , newIncorrect
    , totalChosen
    , withContent
    , withExplanation
    )

import Dict exposing (Dict)
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


choiceToTuple : GetData -> ( Int, GetData )
choiceToTuple data =
    ( data.id, data )


choicesToDictionary : List GetData -> Dict Int GetData
choicesToDictionary data =
    data
        |> List.map choiceToTuple
        |> Dict.fromList


decodeDictionary : Decoder (Dict Int GetData)
decodeDictionary =
    Decode.list decoder
        |> Decode.map choicesToDictionary


totalChosen : Dict Int GetData -> Int
totalChosen dict =
    dict
        |> Dict.toList
        |> List.map (\( k, v ) -> v.numChosen)
        |> List.sum
