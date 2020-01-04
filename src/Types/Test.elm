module Types.Test exposing (..)

import Architecture.Route as Route exposing (Route)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Types.Choice as Choice
import Types.Question as Question
import Types.Specialty as Specialty exposing (Specialty)
import Types.Stage as Stage exposing (Stage)
import Types.Topic as Topic exposing (Topic)


type alias CompletedQuestion =
    { question : Question.GetDetailData
    , choice : Choice.GetData
    }


type alias SessionData =
    { completed : List CompletedQuestion
    , future : List Int -- future question IDs in the test
    }


{-| Start a new test given a list of IDs.
-}
init : List Int -> SessionData
init future =
    { completed = []
    , future = future
    }


{-| Retrieve the next item from the list of future IDs.
-}
unconsFuture : SessionData -> ( SessionData, Route )
unconsFuture data =
    case data.future of
        [] ->
            -- This was the last question, so finalise it before the report
            ( data |> finalise, Route.Report )

        head :: tail ->
            ( { data | future = tail }, Route.Question head )


{-| Adds a completed question to the test session.
-}
addCompleted : CompletedQuestion -> SessionData -> ( SessionData, Route )
addCompleted question data =
    let
        newSession =
            { data | completed = data.completed ++ [ question ] }
    in
    newSession
        |> unconsFuture


{-| Finalises the completed question by reversing the list.

Completed questions are prepended, so the finalise function simply reverses te list of completed questions.

-}
finalise : SessionData -> SessionData
finalise data =
    { data | completed = List.reverse data.completed }
