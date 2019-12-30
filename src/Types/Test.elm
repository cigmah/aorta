module Types.Test exposing (GetData, SessionData, defaultGetData)

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Types.Specialty as Specialty exposing (Specialty)
import Types.Stage as Stage exposing (Stage)
import Types.Topic as Topic exposing (Topic)


{-| Describes the requested type of questions for a new test.

`GetData` contains parameters that a user can select to describe a new
multiple-choice question test. It can that can be used to get a new set of
questions from the backend by encoding it as query parameters to the relevant
endpoint.

-}
type alias GetData =
    { specialties : List Specialty
    , stages : List Stage
    , topics : List Topic
    , quantity : Int
    }


{-| Parameters for a default test.
-}
defaultGetData : GetData
defaultGetData =
    { specialties = []
    , stages = []
    , topics = []
    , quantity = 10
    }


type alias SessionData =
    { completed :
        List
            { id : Int
            , wasCorrect : Bool
            }
    , future : List Int
    , back : String
    }
