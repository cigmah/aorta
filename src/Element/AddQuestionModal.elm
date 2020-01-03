module Element.AddQuestionModal exposing (..)

{-| A modal which allows you to add a question to an objective.
-}

import Dict exposing (Dict)
import Element.Empty as Empty
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Question as Question


type alias Data msg =
    { question : Question.PostData
    , questionWebData : WebData Question.GetBasicData
    , objectiveTitle : String
    , onClickClose : msg
    , onChangeStem : String -> msg
    , onChangeChoiceContent :
        Int
        -> String
        -> msg -- choices are keyed in a dictionary by ID
    , onChangeChoiceExplanation : Int -> String -> msg
    , onClickAddChoice : msg
    , onClickRemoveChoice : Int -> msg
    , onClickSubmit : msg
    }


isLoading : WebData a -> Bool
isLoading webData =
    case webData of
        Loading ->
            True

        _ ->
            False


element : Data msg -> Html msg
element data =
    section
        [ class "add-question-group" ]
        [ div
            [ class "add-question-modal-background"
            , onClick data.onClickClose
            ]
            []
        , Html.form
            [ class "add-question-modal"
            , onSubmit data.onClickSubmit
            ]
            [ header
                [ class "add-question-modal-header" ]
                [ h1
                    [ class "add-question-modal-header-text" ]
                    [ text "Add a New Question" ]
                , button
                    [ class "add-question-modal-header-close"
                    , onClick data.onClickClose
                    , type_ "button"
                    ]
                    [ text "×" ]
                ]
            , section
                [ class "add-question-modal-body" ]
                [ div
                    [ class "add-question-modal-left" ]
                    [ div
                        [ class "add-question-modal-objective" ]
                        [ label
                            [ class "add-question-modal-objective-label" ]
                            [ text "Learning Objective" ]
                        , div
                            [ class "add-question-modal-objective-title" ]
                            [ text data.objectiveTitle ]
                        ]
                    , div
                        [ class "add-question-modal-stem" ]
                        [ label
                            [ class "add-question-modal-stem-label"
                            , for "question-stem"
                            ]
                            [ text "Question Stem" ]
                        , textarea
                            [ class "add-question-modal-stem-textarea"
                            , onInput data.onChangeStem
                            , id "question-stem"
                            , rows 16
                            , required True
                            , value data.question.stem
                            , placeholder "Question stem..."
                            ]
                            []
                        ]
                    ]
                , div
                    [ class "add-question-modal-right" ]
                    [ ol
                        [ class "add-question-modal-choices" ]
                        (Dict.map (viewChoice data) data.question.choices |> Dict.values)
                    , button
                        [ class "add-question-modal-add-choice-button"
                        , onClick data.onClickAddChoice
                        , type_ "button"
                        ]
                        [ text "Add Distractor" ]
                    ]
                ]
            , footer
                [ class "add-question-modal-footer" ]
                [ button
                    [ type_ "submit"
                    , class "add-question-modal-submit-button"
                    , disabled (isLoading data.questionWebData)
                    ]
                    [ text "Add Question" ]
                , viewResponse data.questionWebData
                ]
            ]
        ]


viewChoice : Data msg -> Int -> Choice.PostData -> Html msg
viewChoice data key choice =
    let
        wrapCloseButton el =
            if key > 1 then
                div
                    [ class "add-question-modal-label-group" ]
                    [ el
                    , button
                        [ class "add-question-modal-delete-choice-button"
                        , onClick (data.onClickRemoveChoice key)
                        , type_ "button"
                        ]
                        [ text "Delete" ]
                    ]

            else
                div
                    [ class "add-question-modal-label-group" ]
                    [ el ]

        choiceLabel =
            if choice.isCorrect then
                label
                    [ class "add-question-modal-label correct" ]
                    [ text "Correct Choice" ]

            else
                label
                    [ class "add-question-modal-label incorrect" ]
                    [ text "Distractor" ]

        explanationPlaceholder =
            if choice.isCorrect then
                "Explanation for why this choice is correct..."

            else
                "Explanation for why this choice is incorrect..."
    in
    li
        [ class "add-question-modal-choice" ]
        [ wrapCloseButton choiceLabel
        , input
            [ placeholder "Choice text..."
            , class "add-question-modal-choice-content"
            , classList [ ( "correct", choice.isCorrect ), ( "incorrect", not choice.isCorrect ) ]
            , onInput (data.onChangeChoiceContent key)
            , value choice.content
            , type_ "text"
            , required True
            ]
            []
        , textarea
            [ placeholder explanationPlaceholder
            , class "add-question-modal-choice-explanation"
            , classList [ ( "correct", choice.isCorrect ), ( "incorrect", not choice.isCorrect ) ]
            , onInput (data.onChangeChoiceExplanation key)
            , value choice.explanation
            , required False
            , rows 3
            ]
            []
        ]


viewResponse : WebData Question.GetBasicData -> Html msg
viewResponse webData =
    case webData of
        Success _ ->
            div
                [ class "add-question-modal-response success" ]
                [ text "Thank you! Your question was successfully added." ]

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
                                    "The requested resource wasn't found. Let us know, this shouldn't happen."

                                _ ->
                                    "There was a bad status with code " ++ String.fromInt int ++ "."

                        BadBody string ->
                            "We had a decoding error. Let us know, this shouldn't happen."
            in
            div
                [ class "add-question-modal-response error" ]
                [ text errorString ]

        _ ->
            Empty.element
