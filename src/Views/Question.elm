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
    section [ class "modal question-modal" ]
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
    article [ id "question" ]
        [ header
            [ classList
                [ ( "correct-bg", isCorrect )
                , ( "incorrect-bg", isIncorrect )
                ]
            , tailwind
                [ "items-center", "flex", "transition" ]
            ]
            [ h1
                []
                [ text ("Question #" ++ String.fromInt data.id) ]
            , button [ onClick msgs.clickedClose ]
                [ i [ class "material-icons" ] [ text "close" ] ]
            ]
        , section []
            [ div
                [ tailwind
                    [ "mb-4" ]
                , class "markdown"
                ]
                (Markdown.toHtml Nothing data.stem)
            , div
                [ tailwind
                    [ "flex", "flex-col" ]
                ]
                (List.map (viewChoiceRead msgs modalData.state) data.choices)
            ]
        , footer
            [ classList
                [ ( "opacity-0", modalData.state == Unanswered )
                , ( "correct-bg", isCorrect )
                , ( "incorrect-bg", isIncorrect )
                ]
            , tailwind
                [ "transition", "opacity-1" ]
            ]
            [ button
                [ onClick msgs.clickedFlag
                , tailwind [ "mx-1" ]
                , type_ "button"
                ]
                [ span [ class "material-icons" ] [ text "flag" ] ]
            , button
                [ onClick msgs.clickedLike
                , tailwind [ "mx-1" ]
                , type_ "button"
                ]
                [ span [ class "material-icons" ] [ text "thumb_up" ]
                , numLikesInfo
                ]
            , button
                [ onClick msgs.nextQuestion
                , tailwind [ "mx-1" ]
                , type_ "button"
                ]
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
                , tailwind
                    [ "my-1"
                    , "flex"
                    , "justify-start"
                    , "hover:bg-blue-500"
                    , "hover:text-white"
                    , "p-2"
                    ]
                ]
                [ span [] [ text choice.content ] ]

        Answered chosen webData ->
            let
                opened =
                    chosen.id == choice.id

                openedString =
                    if opened then
                        "open"

                    else
                        "closed"

                -- Dummy
            in
            details
                [ tailwind [ "my-1", "rounded-b" ]
                , classList
                    [ ( "bg-green-200", choice.isCorrect )
                    , ( "bg-red-200", not choice.isCorrect && opened )
                    , ( "bg-gray-200", not choice.isCorrect && not opened )
                    ]
                , Html.Attributes.attribute openedString ""
                ]
                [ summary
                    [ class "choice"
                    , classList
                        [ ( "bg-red-500 text-white", not choice.isCorrect && opened )
                        , ( "bg-green-500 text-white", choice.isCorrect )
                        , ( "bg-gray-300 text-gray-800", not choice.isCorrect && not opened )
                        ]
                    , tailwind
                        [ "flex"
                        , "p-2"
                        , "rounded"
                        , "items-center"
                        ]
                    ]
                    [ span [] [ text choice.content ]
                    , span [ tailwind [ "ml-auto" ] ] [ text (String.fromInt choice.numChosen ++ " other users.") ]
                    ]
                , div
                    [ class "markdown"
                    , tailwind [ "px-2", "py-1" ]
                    ]
                    (Markdown.toHtml Nothing choice.explanation)
                ]


choiceCorrectToString : Bool -> String
choiceCorrectToString correct =
    if correct then
        "correct"

    else
        "incorrect"


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


viewCommentChatStyle : Comment.ReadData -> Html msg
viewCommentChatStyle data =
    div
        [ tailwind
            [ "mb-2"
            , "p-2"
            , "bg-gray-200"
            , "w-full"
            , "text-gray-800"
            , "rounded-lg"
            , "text-sm"
            ]
        ]
        [ label
            [ tailwind [ "font-bold", "text-gray-600", "text-xs" ] ]
            [ text (data.author.username ++ " @ " ++ Datetime.posixToString data.created_at) ]
        , div [ class "markdown", tailwind [ "w-full" ] ]
            (Markdown.toHtml Nothing data.content)
        ]


viewQuestionComments : QuestionMsgs msg -> ModalQuestionData -> Question.ReadData -> Html msg
viewQuestionComments msgs modalData question =
    case modalData.state of
        Unanswered ->
            article [ id "comments" ] []

        Answered readDataChoice webData ->
            let
                backgroundColor =
                    case readDataChoice.isCorrect of
                        True ->
                            "correct-bg"

                        False ->
                            "incorrect-bg"
            in
            article [ id "comments" ]
                [ section []
                    [ div [ id "comments" ]
                        (List.map viewCommentChatStyle question.comments)
                    ]
                , footer
                    [ tailwind
                        [ backgroundColor ]
                    ]
                    [ textarea
                        [ placeholder "Comment here."
                        , value modalData.comment
                        , onInput msgs.changedComment
                        , tailwind [ "text-black" ]
                        , required True
                        ]
                        []
                    , button [ onClick msgs.submitComment, tailwind [ "ml-1" ] ] [ text "Submit" ]
                    ]
                ]
