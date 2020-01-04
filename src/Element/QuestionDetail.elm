module Element.QuestionDetail exposing (..)

{-| A full, detailed view of a question during a test.
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
    { questionWebData : WebData Question.GetDetailData
    , test : Maybe Test.SessionData
    , selected : Maybe Choice.GetData
    , rating : Maybe Int
    , comment : String
    , showObjective : Bool
    , onClickReload : msg
    , onClickChoice : Int -> msg
    , responseWebData : WebData ()
    , onClickRating : Int -> msg
    , ratingWebData : WebData ()
    , onChangeComment : String -> msg
    , onClickSubmitComment : msg
    , commentWebData : WebData Comment.GetData
    , onToggleShowObjective : msg
    , onClickExit : msg
    , onClickNextQuestion : msg
    }


element : Data msg -> Html msg
element data =
    section
        [ class "question-detail-group" ]
        [ section
            [ class "question-detail-main" ]
            [ questionHeader data
            , section
                [ class "question-detail-body" ]
                [ mainQuestion data
                , sidePanel data
                ]
            ]
        , actionFooter data
        ]


type Progress
    = Correct
    | Incorrect
    | Incomplete
    | Current


progressToClass : Progress -> String
progressToClass progress =
    case progress of
        Correct ->
            "correct"

        Incorrect ->
            "incorrect"

        Incomplete ->
            "incomplete"

        Current ->
            "current"


progressSquare : Progress -> Html msg
progressSquare progress =
    div
        [ class "progress-square"
        , classList [ ( progressToClass progress, True ) ]
        ]
        []


renderPast : Test.CompletedQuestion -> Html msg
renderPast question =
    if question.choice.isCorrect then
        progressSquare Correct

    else
        progressSquare Incorrect


questionHeader : Data msg -> Html msg
questionHeader data =
    case data.test of
        Just test ->
            let
                pastSquares =
                    test.completed
                        |> List.map renderPast

                futureSquares =
                    test.future
                        |> List.map (\_ -> progressSquare Incomplete)

                currentSquare =
                    progressSquare Current
            in
            header
                [ class "question-detail-header" ]
                [ div
                    [ class "question-detail-progress-squares" ]
                    (List.concat [ pastSquares, [ currentSquare ], futureSquares ])
                , button
                    [ class "question-detail-close-button"
                    , onClick data.onClickExit
                    ]
                    [ text "×" ]
                ]

        Nothing ->
            header
                [ class "question-detail-header" ]
                [ button
                    [ class "question-detail-close-button"
                    , onClick data.onClickExit
                    ]
                    [ text "×" ]
                ]


mainQuestion : Data msg -> Html msg
mainQuestion data =
    case data.questionWebData of
        Success question ->
            let
                totalChosen =
                    Choice.totalChosen question.choices
            in
            article
                [ class "question-detail-question" ]
                [ header
                    [ class "question-detail-tags" ]
                    [ div
                        [ class "question-detail-tag stage" ]
                        [ text (Stage.enumerable.toBriefString question.objective.stage) ]
                    , div
                        [ class "question-detail-tag specialty" ]
                        [ text (Specialty.enumerable.toBriefString question.objective.specialty) ]
                    , div
                        [ class "question-detail-tag topic" ]
                        [ text (Topic.enumerable.toBriefString question.objective.topic) ]
                    , div
                        [ class "question-detail-tag contributor" ]
                        [ text ("Contributed by " ++ question.contributor.username) ]
                    ]
                , section
                    [ class "question-detail-content" ]
                    [ section
                        [ class "question-detail-stem markdown" ]
                        (Markdown.toHtml Nothing question.stem)
                    , ul
                        [ class "question-detail-choices" ]
                        (question.choices |> Dict.map (renderChoice data data.selected totalChosen) |> Dict.values)
                    ]
                ]

        Loading ->
            article
                [ class "question-detail-question" ]
                [ section
                    [ class "question-detail-loading" ]
                    [ div
                        [ class "loading-box" ]
                        []
                    ]
                ]

        Failure e ->
            let
                errorString =
                    case e of
                        BadUrl string ->
                            "The requested URL was invalid. Let us know, this shouldn't happen. URL requested was: " ++ string

                        Timeout ->
                            "The request timed out. We might be having some downtime, so let us know or check back later."

                        NetworkError ->
                            "We couldn't connect. We might be having some downtime, so let us know or check back later."

                        BadStatus int ->
                            case int of
                                409 ->
                                    "We already have some of the information you entered, so there was a clash. Try changing your inputs."

                                404 ->
                                    "We didn't find any results which matched your criteria. Sorry! We're adding content as fast as we can."

                                _ ->
                                    "There was a bad status with code " ++ String.fromInt int ++ "."

                        BadBody string ->
                            "We had a decoding error. Let us know, this shouldn't happen."
            in
            article
                [ class "question-detail-question" ]
                [ section
                    [ class "question-detail-failure" ]
                    [ text errorString ]
                , button
                    [ class "question-detail-reload"
                    , onClick data.onClickReload
                    ]
                    [ text "Reload?" ]
                ]

        -- impossible state
        NotAsked ->
            Empty.element


renderChoice : Data msg -> Maybe Choice.GetData -> Int -> Int -> Choice.GetData -> Html msg
renderChoice data selectedMaybe totalChosen choiceId choice =
    case selectedMaybe of
        Nothing ->
            li
                [ class "question-detail-choice-item" ]
                [ button
                    [ class "question-detail-choice-button"
                    , onClick (data.onClickChoice choiceId)
                    ]
                    (Markdown.toHtml Nothing choice.content)
                ]

        Just selected ->
            let
                chosenPercent =
                    if totalChosen > 0 then
                        toFloat choice.numChosen / toFloat totalChosen * 100

                    else
                        0
            in
            li
                [ class "question-detail-choice-item" ]
                [ details
                    [ class "question-detail-choice-details"
                    , classList
                        [ ( "correct", choice.isCorrect )
                        , ( "incorrect", not choice.isCorrect )
                        , ( "selected", selected.id == choiceId )
                        ]
                    ]
                    [ summary
                        [ class "question-detail-choice-summary" ]
                        [ div
                            [ class "question-detail-choice-summary-bar"
                            , style "width" (String.fromFloat chosenPercent ++ "%")
                            ]
                            []
                        , div
                            [ class "question-detail-choice-summary-text" ]
                            [ div
                                [ class "question-detail-choice-summary-markdown markdown" ]
                                (Markdown.toHtml Nothing choice.content)
                            , div
                                [ class "question-detail-choice-summary-text-percent" ]
                                [ text (String.append (String.fromFloat chosenPercent |> String.left 4) "%") ]
                            ]
                        ]
                    , section
                        [ class "question-detail-choice-detail" ]
                        (Markdown.toHtml Nothing choice.explanation)
                    ]
                ]


sidePanel : Data msg -> Html msg
sidePanel data =
    case ( data.selected, data.questionWebData ) of
        ( Just selected, Success question ) ->
            article
                [ class "question-detail-side-panel" ]
                [ div
                    [ class "question-detail-objective" ]
                    [ label
                        [ class "question-detail-objective-label" ]
                        [ text "Learning Objective" ]
                    , button
                        [ class "question-detail-objective-result"
                        , onClick data.onToggleShowObjective
                        ]
                        [ figure
                            [ class "objective-result-icon" ]
                            [ Specialty.toIcon question.objective.specialty ]
                        , section
                            [ class "objective-result-body" ]
                            [ header
                                [ class "objective-result-tags" ]
                                [ div
                                    [ class "objective-result-tag stage" ]
                                    [ text <| Stage.enumerable.toBriefString question.objective.stage ]
                                , div
                                    [ class "objective-result-tag specialty" ]
                                    [ text <| Specialty.enumerable.toBriefString question.objective.specialty ]
                                , div
                                    [ class "objective-result-tag topic" ]
                                    [ text <| Topic.enumerable.toBriefString question.objective.topic ]
                                ]
                            , div
                                [ class "objective-result-title" ]
                                [ text question.objective.title ]
                            ]
                        ]
                    ]
                , div
                    [ class "question-detail-rating" ]
                    [ label
                        [ class "question-detail-rating-label" ]
                        [ text "Average Rating" ]
                    , averageRating question.averageRating
                    ]
                , div
                    [ class "question-detail-comments" ]
                    (label [ class "question-detail-comment-label" ] [ text "Comments" ]
                        :: List.map viewComment question.comments
                    )
                , Form.element
                    { onSubmit = data.onClickSubmitComment
                    , submitButtonText = "Submit Comment"
                    , responseWebData = data.commentWebData
                    , children =
                        [ textarea
                            [ class "question-detail-comment-textarea"
                            , onInput data.onChangeComment
                            , rows 4
                            , required True
                            , placeholder "Write a comment here..."
                            , value data.comment
                            ]
                            []
                        ]
                    }
                ]

        _ ->
            article
                [ class "question-detail-side-panel-placeholder" ]
                []


filledStar =
    div
        [ class "filled star" ]
        [ text "★" ]


emptyStar =
    div
        [ class "empty star" ]
        [ text "☆" ]


averageRating : Float -> Html msg
averageRating rating =
    if rating == 0 then
        -- indicates there are no ratings
        div
            [ class "question-detail-rating-rating" ]
            [ text "This question hasn't been rated yet." ]

    else
        let
            roundedRating =
                floor rating

            stars =
                List.concat
                    [ List.repeat roundedRating filledStar
                    , List.repeat (5 - roundedRating) emptyStar
                    ]
        in
        div
            [ class "question-detail-rating-rating" ]
            [ div
                [ class "question-detail-rating-numerical" ]
                [ text ((rating |> String.fromFloat |> String.left 4) ++ " star(s)") ]
            , div
                [ class "question-detail-rating-stars" ]
                stars
            ]


viewComment : Comment.GetData -> Html msg
viewComment comment =
    div
        [ class "question-detail-comment" ]
        [ div
            [ class "question-detail-comment-datetime" ]
            [ text (Datetime.posixToString comment.createdAt) ]
        , div
            [ class "question-detail-comment-content" ]
            (Markdown.toHtml Nothing comment.content)
        ]


actionFooter : Data msg -> Html msg
actionFooter data =
    case data.selected of
        Just selected ->
            footer
                [ class "question-detail-footer"
                , classList [ ( "correct", selected.isCorrect ), ( "incorrect", not selected.isCorrect ) ]
                ]
                [ rateThisQuestion data
                , nextButton data
                ]

        Nothing ->
            Empty.element


rateThisQuestion : Data msg -> Html msg
rateThisQuestion data =
    let
        ratingStarsElement =
            case data.rating of
                Just rating ->
                    List.concat
                        [ List.repeat rating filledStar
                        , List.repeat (5 - rating) emptyStar
                        ]

                _ ->
                    ratingStars data
    in
    div
        [ class "question-detail-rate-question" ]
        [ label
            [ class "question-detail-rate-question-label" ]
            [ text "Rate this Question" ]
        , div
            [ class "question-detail-rate-question-stars" ]
            ratingStarsElement
        ]


ratingStars : Data msg -> List (Html msg)
ratingStars data =
    let
        starButton int =
            button
                [ onClick (data.onClickRating int)
                , class "question-detail-rate-question-star"
                ]
                [ emptyStar ]
    in
    [ 1, 2, 3, 4, 5 ]
        |> List.map starButton


nextButton : Data msg -> Html msg
nextButton data =
    let
        buttonText =
            case data.test of
                Just test ->
                    if List.length test.future == 0 then
                        "Review Results"

                    else
                        "Next Question"

                _ ->
                    "Finish"
    in
    button
        [ type_ "button"
        , onClick data.onClickNextQuestion
        , class "question-detail-next-question-button"
        ]
        [ text buttonText ]


modalBackground : msg -> Html msg
modalBackground onClickMsg =
    div
        [ class "modal-background"
        , onClick onClickMsg
        ]
        []


objectiveModal : Data msg -> Question.GetDetailData -> Html msg
objectiveModal data question =
    if data.showObjective then
        section
            [ class "question-detail-objective-modal-group" ]
            [ modalBackground data.onToggleShowObjective
            , article
                [ class "question-detail-objective-modal" ]
                [ header
                    [ class "question-detail-objective-modal-header" ]
                    [ button
                        [ class "question-detail-objective-modal-close"
                        , onClick data.onToggleShowObjective
                        ]
                        [ text "×" ]
                    ]
                , section
                    [ class "question-detail-objective-modal-body" ]
                    [ h1
                        [ class "question-detail-objective-modal-title" ]
                        [ text question.objective.title ]
                    , div
                        [ class "question-detail-objective-modal-notes" ]
                        (Markdown.toHtml Nothing question.objective.notes)
                    ]
                ]
            ]

    else
        Empty.element
