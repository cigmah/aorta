module Views.Question exposing
    ( ModalQuestionData
    , QuestionMsgs
    , QuestionState(..)
    , viewChoiceRead
    , viewComment
    , viewQuestion
    , viewQuestionComments
    , viewQuestionSection
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Datetime as Datetime
import Types.Question as Question
import Types.Styles exposing (tailwind)


type alias ModalQuestionData =
    { questionId : Int
    , webData : WebData Question.ReadData
    , state : QuestionState
    , comment : String
    , commentResponse : WebData Comment.ReadData
    , likeResponse : WebData ()
    , flagResponse : WebData ()
    }


type QuestionState
    = Unanswered
    | Answered Choice.ReadData (WebData ())


type alias QuestionMsgs msg =
    { clickedLike : msg
    , clickedFlag : msg
    , nextQuestion : msg
    , clickedChoice : Choice.ReadData -> msg
    , changedComment : String -> msg
    , submitComment : msg
    , clickedClose : msg
    }


viewQuestionSection : QuestionMsgs msg -> ModalQuestionData -> Question.ReadData -> Html msg
viewQuestionSection msgs modalData question =
    section [ id "question-modal" ]
        [ viewQuestion msgs modalData question
        , viewQuestionComments msgs modalData question
        ]


viewQuestion : QuestionMsgs msg -> ModalQuestionData -> Question.ReadData -> Html msg
viewQuestion msgs modalData data =
    let
        ( isCorrect, isIncorrect ) =
            case modalData.state of
                Unanswered ->
                    ( False, False )

                Answered choice _ ->
                    if choice.isCorrect then
                        ( True, False )

                    else
                        ( False, True )

        numLikesInfo =
            case data.numLikes of
                Just int ->
                    span [] [ text (String.fromInt int) ]

                Nothing ->
                    span [] []
    in
    article []
        [ header
            [ classList
                [ ( "correct", isCorrect )
                , ( "incorrect", isIncorrect )
                ]
            ]
            [ h1
                []
                [ text ("Question #" ++ String.fromInt data.id) ]
            , button [ onClick msgs.clickedClose ]
                [ i [ class "material-icons" ] [ text "close" ] ]
            ]
        , section []
            [ div [ id "stem" ]
                (Markdown.toHtml Nothing data.stem)
            , div [ id "choices" ]
                (List.map (viewChoiceRead msgs modalData.state) data.choices)
            ]
        , footer
            [ classList
                [ ( "hide-under", modalData.state == Unanswered )
                , ( "correct", isCorrect )
                , ( "incorrect", isIncorrect )
                ]
            ]
            [ button
                [ onClick msgs.clickedFlag ]
                [ span [ class "material-icons" ] [ text "flag" ] ]
            , button
                [ onClick msgs.clickedLike ]
                [ span [ class "material-icons" ] [ text "thumb_up" ]
                , numLikesInfo
                ]
            , button
                [ onClick msgs.nextQuestion ]
                [ text "Next Question" ]
            ]
        ]


viewChoiceRead : QuestionMsgs msg -> QuestionState -> Choice.ReadData -> Html msg
viewChoiceRead msgs state choice =
    case state of
        Unanswered ->
            button
                [ class "choice"
                , onClick (msgs.clickedChoice choice)
                ]
                [ span [] [ text choice.content ] ]

        Answered chosen webData ->
            div
                [ class "choice"
                , classList
                    [ ( "incorrect", not choice.isCorrect && (chosen.id == choice.id) )
                    , ( "correct", choice.isCorrect )
                    ]
                ]
                [ span [] [ text choice.content ]
                , span [ class "chosen-by" ] [ text (String.fromInt choice.numChosen ++ " other users.") ]
                ]


viewComment : Comment.ReadData -> Html msg
viewComment data =
    div
        [ tailwind
            [ "mb-2", "pb-2", "text-sm" ]
        ]
        [ label
            [ tailwind
                [ "text-xs"
                , "text-gray-700"
                ]
            ]
            [ text
                (String.join
                    " "
                    [ data.author.username, "on", Datetime.posixToString data.created_at ]
                )
            ]
        , div
            [ class "markdown"
            , tailwind
                [ "pl-3", "border-l", "border-dotted", "border-gray-600" ]
            ]
            (Markdown.toHtml Nothing data.content)
        ]


viewQuestionComments : QuestionMsgs msg -> ModalQuestionData -> Question.ReadData -> Html msg
viewQuestionComments msgs modalData question =
    case modalData.state of
        Unanswered ->
            article [] []

        Answered readDataChoice webData ->
            article [ id "content" ]
                [ section []
                    [ div [ id "comments" ]
                        (List.map viewComment question.comments)
                    ]
                , footer []
                    [ textarea
                        [ placeholder "Comment here."
                        , value modalData.comment
                        , onInput msgs.changedComment
                        ]
                        []
                    , button [ onClick msgs.submitComment ] [ text "Submit" ]
                    ]
                ]
