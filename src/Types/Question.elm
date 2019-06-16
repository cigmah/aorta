module Types.Question exposing
    ( Question
    , decoder
    , encode
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Choice as Choice exposing (Choice, ChoiceBase)
import Types.Comment as Comment exposing (Comment)
import Types.Tag as Tag exposing (Tag, TagBase)
import Types.User as User exposing (User)


type alias QuestionBase =
    { tags : List TagBase
    , stem : String
    , answer : ChoiceBase
    , distractors : List ChoiceBase
    , explanation : String
    }


type alias Question =
    { id : Int
    , userId : User
    , tags : List Tag
    , stem : String
    , answer : Choice
    , distractors : List Choice
    , explanation : String
    , likes : List User
    , comments : List Comment
    , flagged : Bool
    , timestamp : Posix
    }



-- Json


decoder : Decoder Question
decoder =
    Decode.succeed Question
        |> required "id" Decode.int
        |> required "user_id" User.decoder
        |> required "tags" (Decode.list Tag.decoder)
        |> required "stem" Decode.string
        |> required "answer" Choice.decoder
        |> required "distractors" (Decode.list Choice.decoder)
        |> required "explanation" Decode.string
        |> required "likes" (Decode.list User.decoder)
        |> required "comments" (Decode.list Comment.decoder)
        |> required "flagged" Decode.bool
        |> required "timestamp" Iso8601.decoder


encode : QuestionBase -> Value
encode question =
    Encode.object
        [ ( "tags", Encode.list Tag.encode question.tags )
        , ( "stem", Encode.string question.stem )
        , ( "answer", Choice.encode question.answer )
        , ( "distractors", Encode.list Choice.encode question.distractors )
        , ( "explanation", Encode.string question.explanation )
        ]
