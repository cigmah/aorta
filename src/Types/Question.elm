module Types.Question exposing (..)

{-| A multiple-choice question - the core question-bank feature of AORTA.

Each question has a parent objective, which is tagged with a specialty, topic
and medical training stage.

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Datetime as Datetime
import Types.Objective as Objective
import Types.Specialty as Specialty exposing (Specialty(..))
import Types.Stage as Stage exposing (Stage(..))
import Types.Topic as Topic exposing (Topic(..))
import Types.User as User


{-| Basic question data without the choices.

This is used for viewing questions on the parent objective page.

-}
type alias GetBasicData =
    { id : Int
    , objectiveId : Int
    , contributor : User.GetData
    , createdAt : Posix
    , modifiedAt : Posix
    , stem : String
    }


{-| Decode basic question data without the choices.
-}
decoderBasic : Decoder GetBasicData
decoderBasic =
    Decode.succeed GetBasicData
        |> required "id" Decode.int
        |> required "objective" Decode.int
        |> required "contributor" User.decoder
        |> required "created_at" Datetime.decoder
        |> required "modified_at" Datetime.decoder
        |> required "stem" Decode.string
