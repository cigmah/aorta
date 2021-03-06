module Types.Objective exposing (..)

{-| A medical learning objective.

All questions have a parent learning objective.

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Datetime as Datetime
import Types.Stage as Stage exposing (Stage(..))
import Types.System as System exposing (System(..))
import Types.Topic as Topic exposing (Topic(..))
import Types.User as User


{-| Data required for creating an objective.

For simplicity sake, default the notes to empty string on creation.

-}
type alias PostData =
    { system : System
    , stage : Stage
    , topic : Topic
    , title : String
    }


{-| Initial default creation data for an objective.
-}
init : PostData
init =
    { system = Principles
    , stage = Year1
    , topic = Overview
    , title = ""
    }


updateSystem : System -> PostData -> PostData
updateSystem system data =
    { data | system = system }


updateStage : Stage -> PostData -> PostData
updateStage stage data =
    { data | stage = stage }


updateTopic : Topic -> PostData -> PostData
updateTopic topic data =
    { data | topic = topic }


updateTitle : String -> PostData -> PostData
updateTitle title data =
    { data | title = title }


type alias GetData =
    { id : Int
    , system : System
    , topic : Topic
    , stage : Stage
    , title : String
    , notes : String
    , createdAt : Posix
    , modifiedAt : Posix
    , num_questions : Int
    , contributor : User.GetData
    }


decoder : Decoder GetData
decoder =
    Decode.succeed GetData
        |> required "id" Decode.int
        |> required "specialty" System.enumerable.decoder
        -- legacy - systems were initially called specialties
        |> required "topic" Topic.enumerable.decoder
        |> required "stage" Stage.enumerable.decoder
        |> required "title" Decode.string
        |> required "notes" Decode.string
        |> required "created_at" Datetime.decoder
        |> required "modified_at" Datetime.decoder
        |> required "num_questions" Decode.int
        |> required "contributor" User.decoder


encode : PostData -> Value
encode data =
    Encode.object
        [ ( "specialty", System.enumerable.encode data.system ) -- legacy - systems were initially called specialties
        , ( "topic", Topic.enumerable.encode data.topic )
        , ( "stage", Stage.enumerable.encode data.stage )
        , ( "title", Encode.string data.title )
        , ( "notes", Encode.string "" )
        ]


type alias EditableData =
    { title : String
    , notes : String
    }


editableFromData : GetData -> EditableData
editableFromData data =
    { title = data.title
    , notes = data.notes
    }


encodeEditable : EditableData -> Value
encodeEditable data =
    Encode.object
        [ ( "title", Encode.string data.title )
        , ( "notes", Encode.string data.notes )
        ]
