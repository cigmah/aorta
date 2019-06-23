module Types.Test exposing (CompletedQuestion, Test, decode, decoderCompleted, encode, encodeCompleted)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type alias Test =
    { remaining : List Int
    , completed : List CompletedQuestion
    }


type alias CompletedQuestion =
    { questionId : Int
    , correct : Bool
    }


encodeCompleted : CompletedQuestion -> Value
encodeCompleted data =
    Encode.object
        [ ( "question_id", Encode.int data.questionId )
        , ( "correct", Encode.bool data.correct )
        ]


encode : Test -> Value
encode data =
    Encode.object
        [ ( "remaining", Encode.list Encode.int data.remaining )
        , ( "completed", Encode.list encodeCompleted data.completed )
        ]


decoderCompleted : Decoder CompletedQuestion
decoderCompleted =
    Decode.map2 CompletedQuestion
        (Decode.field "question_id" Decode.int)
        (Decode.field "correct" Decode.bool)


decode : Decoder Test
decode =
    Decode.map2 Test
        (Decode.field "remaining" (Decode.list Decode.int))
        (Decode.field "completed" (Decode.list decoderCompleted))
