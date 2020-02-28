module Types.Test exposing (..)

import Architecture.Route as Route exposing (Route)
import Dict
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Types.Choice as Choice
import Types.Question as Question
import Types.Stage as Stage exposing (Stage)
import Types.System as System exposing (System)
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


type Grade
    = A
    | B
    | C
    | D
    | E
    | F


gradeToLetter : Grade -> String
gradeToLetter grade =
    case grade of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"


{-| Converts an int in range 0 to 100 to a letter grade.
-}
scoreToGrade : Int -> Grade
scoreToGrade score =
    if score > 90 then
        A

    else if score > 80 then
        B

    else if score > 70 then
        C

    else if score > 60 then
        D

    else if score > 50 then
        E

    else
        F


{-| Calculates the percentage correct and a grade for a test.
-}
calculateScore : SessionData -> ( Int, Grade )
calculateScore test =
    let
        numCorrect =
            test.completed
                |> List.filter (\question -> question.choice.isCorrect)
                |> List.length

        numIncorrect =
            test.completed
                |> List.length

        percentScore =
            (numCorrect * 100) // numIncorrect

        grade =
            scoreToGrade percentScore
    in
    ( percentScore, grade )
