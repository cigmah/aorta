module Element.TestReport exposing (..)

{-| A test report shown at the end of the test session.
-}

import Dict exposing (Dict)
import Element.Empty as Empty
import Element.Form as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Markdown
import Maybe.Extra
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Datetime as Datetime
import Types.Objective as Objective
import Types.Question as Question
import Types.Specialty as Specialty
import Types.Stage as Stage
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
    let
        ( score, grade ) =
            Test.calculateScore test

        gradeString =
            Test.gradeToLetter grade
    in
    section
        [ class "test-report" ]
        [ header
            [ class "test-report-header" ]
            [ div
                [ class <| "test-report-grade"
                , classList [ ( gradeString, True ) ]
                ]
                [ text gradeString ]
            , div
                [ class "test-report-statement"
                , classList [ ( gradeString, True ) ]
                ]
                [ text <| "You scored " ++ String.fromInt score ++ "%." ]
            ]
        , button
            [ class "test-report-finish-button"
            , onClick data.onClickFinish
            ]
            [ text "Finish Review" ]
        , section
            [ class "test-report-body" ]
            (List.map renderCompletedQuestion test.completed)
        ]


renderCompletedQuestion : Test.CompletedQuestion -> Html msg
renderCompletedQuestion completed =
    article
        [ class "test-report-completed"
        , classList [ ( "correct", completed.choice.isCorrect ), ( "incorrect", not completed.choice.isCorrect ) ]
        ]
        [ div
            [ class "test-report-completed-stem" ]
            (Markdown.toHtml Nothing completed.question.stem)
        , div
            [ class "test-report-completed-your-answer" ]
            [ div
                [ class "test-report-completed-your-answer-text" ]
                [ text "You answered: " ]
            , div
                [ class "test-report-completed-your-answer-choice" ]
                (Markdown.toHtml Nothing completed.choice.content)
            ]
        ]
