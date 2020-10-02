module Element.TestReport exposing (..)

{-| A test report shown at the end of the test session.
-}

import Dict exposing (Dict)
import Dict.Extra
import Element.Empty as Empty
import Element.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import List
import Maybe
import Maybe.Extra
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Datetime as Datetime
import Types.Markdown exposing (markdown)
import Types.Objective as Objective
import Types.Question as Question
import Types.Stage as Stage
import Types.System as System
import Types.Test as Test
import Types.Topic as Topic


type alias Data msg =
    { test : Maybe Test.SessionData
    , onClickFinish : msg
    }


element : Data msg -> Html msg
element data =
    case data.test of
        Nothing ->
            section
                [ class "test-report-empty" ]
                [ h1
                    [ class "test-report-empty-heading" ]
                    [ text "Uh oh...you shouldn't be here! There's no current test session (and remember that test sessions expire if you leave a page). Go back home?" ]
                , button
                    [ class "test-report-empty-button"
                    , onClick data.onClickFinish
                    ]
                    [ text "Take me home!" ]
                ]

        Just test ->
            viewTestResults data test


viewTestResults : Data msg -> Test.SessionData -> Html msg
viewTestResults data test =
    section
        [ class "test-report" ]
        [ section
            [ class "test-report-left" ]
            (h1 [ class "test-report-left-heading" ] [ text "Question Review" ]
                :: List.map renderCompletedQuestion test.completed
            )
        , section
            [ class "test-report-right" ]
            (testStatistics data test)
        ]


getCorrectChoiceString : Test.CompletedQuestion -> ( String, String )
getCorrectChoiceString completed =
    completed.question.choices
        |> Dict.values
        |> List.filter (\c -> c.isCorrect)
        |> List.head
        |> Maybe.map (\c -> ( c.content, c.explanation ))
        |> Maybe.withDefault ( "", "" )


renderCompletedQuestion : Test.CompletedQuestion -> Html msg
renderCompletedQuestion completed =
    let
        markingSymbol =
            if completed.choice.isCorrect then
                div [ class "test-report-completed-symbol correct" ] [ text "✓" ]

            else
                div [ class "test-report-completed-symbol incorrect" ] [ text "✗" ]

        ( correctAnswer, correctExplanation ) =
            if completed.choice.isCorrect then
                ( Empty.element, Empty.element )

            else
                let
                    ( content, explanation ) =
                        getCorrectChoiceString completed
                in
                ( div
                    [ class "test-report-completed-correct-answer" ]
                    [ div
                        [ class "test-report-completed-correct-answer-text" ]
                        [ text "The correct answer is: " ]
                    , div
                        [ class "test-report-completed-correct-answer-choice" ]
                        [ markdown content ]
                    ]
                , div
                    [ class "test-report-completed-correct-explanation markdown" ]
                    [ markdown explanation ]
                )
    in
    article
        [ class "test-report-completed"
        , classList [ ( "correct", completed.choice.isCorrect ), ( "incorrect", not completed.choice.isCorrect ) ]
        ]
        [ div
            [ class "test-report-completed-stem markdown" ]
            [ markdown completed.question.stem ]
        , div
            [ class "test-report-completed-your-answer" ]
            [ div
                [ class "test-report-completed-your-answer-text" ]
                [ text "You answered: " ]
            , div
                [ class "test-report-completed-your-answer-choice" ]
                [ markdown completed.choice.content ]
            , markingSymbol
            ]
        , div
            [ class "test-report-completed-explanation markdown" ]
            [ markdown completed.choice.explanation ]
        , correctAnswer
        , correctExplanation
        ]


getCorrectChoicePercent : Dict Int Choice.GetData -> Float
getCorrectChoicePercent choices =
    let
        totalChosen =
            choices
                |> Dict.values
                |> List.map .numChosen
                |> List.sum

        correctChosen =
            choices
                |> Dict.values
                |> List.filter (\c -> c.isCorrect)
                |> List.head
                |> Maybe.map .numChosen
                |> Maybe.withDefault 0
    in
    if totalChosen == 0 then
        0

    else
        100 * toFloat correctChosen / toFloat totalChosen


type alias SummaryScore =
    { right : Int
    , wrong : Int
    }


{-| Convert a list of (system/topic ID, correct/incorrect answer) tuples into a summary score.
-}
toSummaryScore : List ( Int, Bool ) -> SummaryScore
toSummaryScore tupleList =
    tupleList
        |> List.map (\( k, bool ) -> bool)
        |> (\l ->
                { right = l |> List.filter identity |> List.length
                , wrong = l |> List.filter (identity >> not) |> List.length
                }
           )


scoresBySystem : Test.SessionData -> List ( String, SummaryScore )
scoresBySystem test =
    let
        toKeyCorrectTuple completed =
            ( System.enumerable.toInt completed.question.objective.system
            , completed.choice.isCorrect
            )
    in
    test.completed
        |> List.map toKeyCorrectTuple
        |> Dict.Extra.groupBy (\( key, _ ) -> key)
        |> Dict.toList
        |> List.map (\( key, grouped ) -> ( key, toSummaryScore grouped ))
        |> List.sortBy (\( _, summary ) -> toFloat summary.right / toFloat (summary.wrong + 1))
        |> List.reverse
        |> List.map
            (\( key, summary ) ->
                ( key
                    |> System.enumerable.fromInt
                    |> System.enumerable.toBriefString
                , summary
                )
            )


scoresByTopic : Test.SessionData -> List ( String, SummaryScore )
scoresByTopic test =
    let
        toKeyCorrectTuple completed =
            ( Topic.enumerable.toInt completed.question.objective.topic
            , completed.choice.isCorrect
            )
    in
    test.completed
        |> List.map toKeyCorrectTuple
        |> Dict.Extra.groupBy (\( key, _ ) -> key)
        |> Dict.toList
        |> List.map (\( key, grouped ) -> ( key, toSummaryScore grouped ))
        |> List.sortBy (\( _, summary ) -> toFloat summary.right / toFloat (summary.wrong + 1))
        |> List.reverse
        |> List.map
            (\( key, summary ) ->
                ( key
                    |> Topic.enumerable.fromInt
                    |> Topic.enumerable.toBriefString
                , summary
                )
            )


{-| View the aggregated test statistics.
-}
testStatistics : Data msg -> Test.SessionData -> List (Html msg)
testStatistics data test =
    let
        totalQuestions =
            List.length test.completed

        numCorrectQuestions =
            test.completed
                |> List.filter (\q -> q.choice.isCorrect)
                |> List.length

        rawScore =
            round <| 100 * toFloat numCorrectQuestions / toFloat totalQuestions

        avgScore =
            test.completed
                |> List.map (.question >> .choices >> getCorrectChoicePercent)
                |> (\l -> round <| List.sum l / (toFloat <| List.length l))

        systemScores =
            scoresBySystem test

        topicScores =
            scoresByTopic test
    in
    [ section
        [ class "test-report-statistics-your-score" ]
        [ div
            [ class "test-report-statistics-your-score-text" ]
            [ text "Your score: " ]
        , div
            [ class "test-report-statistics-your-score-score" ]
            [ text <| String.fromInt rawScore ++ "%" ]
        ]
    , section
        [ class "test-report-statistics-user-average" ]
        [ div
            [ class "test-report-statistics-user-average-text" ]
            [ text "User average: " ]
        , div
            [ class "test-report-statistics-user-average-score" ]
            [ text <| String.fromInt avgScore ++ "%" ]
        ]
    , button
        [ class "test-report-finish-button"
        , onClick data.onClickFinish
        ]
        [ text "Finish" ]
    , section
        [ class "test-report-statistics-system-scores" ]
        [ renderGroupedScores systemScores ]
    , section
        [ class "test-report-statistics-topic-scores" ]
        [ renderGroupedScores topicScores ]
    ]


renderBlock : ( String, SummaryScore ) -> Html msg
renderBlock ( title, score ) =
    let
        numRight =
            score.right

        numWrong =
            score.wrong

        numTotal =
            score.right + score.wrong

        percentRight =
            round <| 100 * toFloat numRight / toFloat numTotal

        percentWrong =
            100 - percentRight
    in
    div
        [ class "test-report-grouped-scores-block" ]
        [ div
            [ class "test-report-grouped-scores-block-text" ]
            [ text title ]
        , div
            [ class "test-report-grouped-scores-block-fraction" ]
            [ text <| String.join "/" [ String.fromInt numRight, String.fromInt numTotal ] ]
        , div
            [ class "test-report-grouped-scores-block-graphic" ]
            [ div
                [ class "test-report-grouped-scores-block-block correct"
                , style "width" (String.fromInt percentRight ++ "%")
                ]
                []
            , div
                [ class "test-report-grouped-scores-block-block incorrect"
                , style "width" (String.fromInt percentWrong ++ "%")
                ]
                []
            ]
        ]


renderGroupedScores : List ( String, SummaryScore ) -> Html msg
renderGroupedScores scores =
    article
        [ class "test-report-grouped-scores" ]
        (List.map renderBlock scores)
