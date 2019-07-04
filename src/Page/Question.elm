module Page.Question exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

{-| This page is the main EMQ review page for reviewing questions.

This page may be entered by:

1.  Clicking Study from a Note page when items are due
2.  Clicking Study from a Note page when no items are due
3.  Clicking Study from the Revise page to start a session
4.  Clicking Next Question from this page itself
5.  Accessing a URL e.g. ./questions/1/

-}

import Architecture.Route as Route
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown
import Random
import Random.List
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Datetime as Datetime
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Styles exposing (tailwind)
import Types.Test as Test exposing (Test)



-- Model


type alias Model =
    { session : Session
    , questionId : Int
    , webQuestion : WebQuestion
    }


type WebQuestion
    = LoadingQuestion
    | RandomisingQuestion Question.ReadData
    | FailureQuestion Http.Error
    | SuccessQuestion Question.ReadData State


type State
    = Unanswered
    | Answered AnsweredData


type alias AnsweredData =
    { choice : Choice.ReadData
    , responseResponse : WebData ()
    , comment : String
    , commentResponse : WebData Comment.ReadData
    , likeResponse : WebData ()
    , flagResponse : WebData ()
    }


initAnswer : Choice.ReadData -> AnsweredData
initAnswer choice =
    { choice = choice
    , responseResponse = Loading
    , comment = ""
    , commentResponse = NotAsked
    , likeResponse = NotAsked
    , flagResponse = NotAsked
    }



-- Msg


type Msg
    = NoOp
    | ClickedClose
    | GotQuestion (WebData Question.ReadData)
    | RandomisedQuestion (List Choice.ReadData)
    | ClickedChoice Choice.ReadData
    | GotResponseResponse (WebData ())
    | ClickedLike
    | GotLikeResponse (WebData ())
    | ClickedFlag
    | GotFlagResponse (WebData ())
    | ChangedComment String
    | ClickedSubmitComment
    | GotSubmitCommentResponse (WebData Comment.ReadData)
    | ClickedNextQuestion



-- Init


init : Session -> Int -> ( Model, Cmd Msg )
init session questionId =
    ( { session = session
      , questionId = questionId
      , webQuestion = LoadingQuestion
      }
    , Request.get (getQuestion session questionId)
    )



-- Eject


eject : Model -> Session
eject model =
    model.session



-- Inject


inject : Model -> Session -> ( Model, Cmd Msg )
inject model session =
    ( { model | session = session }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Requests


getQuestion : Session -> Int -> Request.GetRequest Question.ReadData Msg
getQuestion session questionId =
    { auth = session.auth
    , endpoint = Request.GetQuestion questionId
    , callback = GotQuestion
    , returnDecoder = Question.decoder
    , queryList = []
    }


postResponse : Session -> Int -> Int -> Request.PostRequest () Msg
postResponse session questionId choiceId =
    { auth = session.auth
    , endpoint = Request.PostResponse
    , callback = GotResponseResponse
    , returnDecoder = Decode.succeed ()
    , queryList = []
    , body =
        Encode.object
            [ ( "question", Encode.int questionId )
            , ( "choice", Encode.int choiceId )
            ]
    }


postLike : Session -> Int -> Request.PostRequest () Msg
postLike session questionId =
    { auth = session.auth
    , endpoint = Request.PostLike
    , callback = GotLikeResponse
    , returnDecoder = Decode.succeed ()
    , queryList = []
    , body =
        Encode.object
            [ ( "question", Encode.int questionId ) ]
    }


postFlag : Session -> Int -> Request.PostRequest () Msg
postFlag session questionId =
    { auth = session.auth
    , endpoint = Request.PostFlag
    , callback = GotFlagResponse
    , returnDecoder = Decode.succeed ()
    , queryList = []
    , body =
        Encode.object
            [ ( "question", Encode.int questionId ) ]
    }


postComment : Session -> Int -> String -> Request.PostRequest Comment.ReadData Msg
postComment session questionId comment =
    { auth = session.auth
    , endpoint = Request.PostQuestionComment
    , callback = GotSubmitCommentResponse
    , returnDecoder = Comment.decoder
    , queryList = []
    , body =
        Encode.object
            [ ( "question", Encode.int questionId )
            , ( "content", Encode.string comment )
            ]
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ session } as model) =
    let
        ignore =
            ( model, Cmd.none )

        exit =
            case session.test of
                Just test ->
                    ( model
                    , Navigation.pushUrl
                        session.key
                        test.back
                    )

                Nothing ->
                    ( model
                    , Navigation.pushUrl
                        session.key
                        (Route.toString Route.Home)
                    )
    in
    case model.webQuestion of
        LoadingQuestion ->
            case msg of
                GotQuestion webData ->
                    case webData of
                        Success question ->
                            ( { model | webQuestion = RandomisingQuestion question }
                            , Random.generate RandomisedQuestion (Random.List.shuffle question.choices)
                            )

                        Failure e ->
                            ( { model | webQuestion = FailureQuestion e }, Cmd.none )

                        _ ->
                            ( { model | webQuestion = FailureQuestion (Http.BadStatus 404) }, Cmd.none )

                ClickedClose ->
                    exit

                _ ->
                    ignore

        RandomisingQuestion question ->
            case msg of
                RandomisedQuestion choices ->
                    ( { model | webQuestion = SuccessQuestion { question | choices = choices } Unanswered }
                    , Cmd.none
                    )

                _ ->
                    ignore

        FailureQuestion errorHttp ->
            case msg of
                ClickedClose ->
                    exit

                _ ->
                    ignore

        SuccessQuestion question state ->
            case state of
                Unanswered ->
                    case msg of
                        ClickedChoice choice ->
                            let
                                newModel newAnswer =
                                    { model
                                        | webQuestion =
                                            SuccessQuestion
                                                question
                                                (Answered newAnswer)
                                    }
                            in
                            case model.session.auth of
                                Guest ->
                                    ( newModel (initAnswer choice), Cmd.none )

                                User user ->
                                    let
                                        initial =
                                            initAnswer choice
                                    in
                                    ( newModel { initial | responseResponse = Loading }
                                    , Request.post (postResponse model.session model.questionId choice.id)
                                    )

                        ClickedClose ->
                            exit

                        _ ->
                            ignore

                Answered data ->
                    let
                        wrap newData =
                            { model | webQuestion = SuccessQuestion question (Answered newData) }
                    in
                    case msg of
                        GotResponseResponse webData ->
                            ( wrap { data | responseResponse = webData }, Cmd.none )

                        ClickedLike ->
                            case data.likeResponse of
                                Loading ->
                                    ignore

                                _ ->
                                    ( wrap { data | likeResponse = Loading }
                                    , Request.post (postLike model.session model.questionId)
                                    )

                        GotLikeResponse webData ->
                            case webData of
                                Success _ ->
                                    ( { model
                                        | webQuestion =
                                            SuccessQuestion
                                                { question
                                                    | numLikes = Maybe.withDefault 0 question.numLikes + 1 |> Just
                                                    , liked = Just True
                                                }
                                                (Answered { data | likeResponse = webData })
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( wrap { data | likeResponse = webData }, Cmd.none )

                        ClickedFlag ->
                            case data.flagResponse of
                                Loading ->
                                    ignore

                                _ ->
                                    ( wrap { data | flagResponse = Loading }
                                    , Request.post (postFlag model.session model.questionId)
                                    )

                        GotFlagResponse webData ->
                            ( wrap { data | flagResponse = webData }, Cmd.none )

                        ChangedComment string ->
                            ( wrap { data | comment = string }, Cmd.none )

                        ClickedSubmitComment ->
                            case data.commentResponse of
                                Loading ->
                                    ignore

                                _ ->
                                    case data.comment of
                                        "" ->
                                            ignore

                                        _ ->
                                            ( wrap { data | commentResponse = Loading }
                                            , Request.post (postComment model.session model.questionId data.comment)
                                            )

                        GotSubmitCommentResponse webData ->
                            case webData of
                                Success newComment ->
                                    ( { model
                                        | webQuestion =
                                            SuccessQuestion
                                                { question | comments = question.comments ++ [ newComment ] }
                                                (Answered
                                                    { data
                                                        | commentResponse = webData
                                                        , comment = ""
                                                    }
                                                )
                                      }
                                    , Cmd.none
                                    )

                                Failure e ->
                                    let
                                        message =
                                            case e of
                                                Http.NetworkError ->
                                                    "There was a network error."

                                                _ ->
                                                    "There was an error submitting your comment. Please try again later."
                                    in
                                    ( { model
                                        | webQuestion = SuccessQuestion question (Answered { data | commentResponse = webData })
                                        , session = Session.addMessage session message
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( wrap { data | commentResponse = webData }, Cmd.none )

                        ClickedClose ->
                            exit

                        ClickedNextQuestion ->
                            case session.test of
                                Just test ->
                                    let
                                        newTest =
                                            { test
                                                | completed =
                                                    test.completed
                                                        ++ [ { id = model.questionId
                                                             , wasCorrect = data.choice.isCorrect
                                                             }
                                                           ]
                                            }
                                    in
                                    case test.future of
                                        [] ->
                                            ( { model | session = { session | test = Just newTest } }
                                            , Navigation.pushUrl
                                                session.key
                                                (Route.toString Route.Finish)
                                            )

                                        head :: tail ->
                                            ( { model
                                                | session = { session | test = Just { newTest | future = tail } }
                                              }
                                            , Navigation.pushUrl session.key (Route.toString (Route.Question head))
                                            )

                                Nothing ->
                                    ( model
                                    , Navigation.pushUrl session.key (Route.toString Route.Home)
                                    )

                        _ ->
                            ignore



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Question"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    let
        loading =
            [ section [ class "modal question-modal" ]
                [ article [ id "question" ]
                    [ viewHeader model Unanswered
                    , section [ tailwind [ "flex", "justify-center", "items-center" ] ]
                        [ div [ class "loading" ] []
                        ]
                    ]
                , article [ id "comments" ] []
                ]
            ]
    in
    case model.webQuestion of
        LoadingQuestion ->
            loading

        RandomisingQuestion question ->
            loading

        FailureQuestion errorHttp ->
            [ section [ class "modal" ]
                [ text "Failure" ]
            ]

        SuccessQuestion question state ->
            [ section [ class "modal question-modal" ]
                [ viewQuestion model state question
                , viewQuestionComments model state question
                ]
            ]



-- View Helpers


viewQuestion : Model -> State -> Question.ReadData -> Html Msg
viewQuestion model state question =
    let
        totalChosen =
            List.map (\choice -> choice.numChosen) question.choices
                |> List.sum
    in
    article [ id "question" ]
        [ viewHeader model state
        , section []
            [ div
                [ tailwind
                    [ "mb-4" ]
                , class "markdown"
                ]
                (Markdown.toHtml Nothing question.stem)
            , div
                [ tailwind
                    [ "flex", "flex-col" ]
                ]
                (List.map (viewChoiceRead totalChosen state) question.choices)
            , div [ tailwind [ "text-gray-500", "text-sm", "text-right", "w-full", "mt-4" ] ]
                [ text "Contributed by "
                , span [ tailwind [ "font-bold" ] ] [ text question.contributor.username ]
                ]
            ]
        , viewFooter model state question
        ]


viewHeader : Model -> State -> Html Msg
viewHeader model state =
    let
        viewComplete : { id : Int, wasCorrect : Bool } -> Html Msg
        viewComplete { id, wasCorrect } =
            div
                [ tailwind [ "h-4", "w-4", "m-px", "rounded" ]
                , classList
                    [ ( "bg-green-500", wasCorrect )
                    , ( "bg-red-500", not wasCorrect )
                    ]
                ]
                []

        viewCurrent =
            case state of
                Unanswered ->
                    div [ tailwind [ "h-4", "w-4", "m-px", "rounded", "border", "bg-white", "border-blue-500" ] ] []

                Answered answeredData ->
                    let
                        color =
                            if answeredData.choice.isCorrect then
                                "bg-green-500"

                            else
                                "bg-red-500"
                    in
                    div [ tailwind [ "h-4", "w-4", "m-px", "rounded", "border", color, "border-gray-400" ] ] []

        viewFuture : a -> Html Msg
        viewFuture _ =
            div [ tailwind [ "h-4", "w-4", "m-px", "rounded", "bg-gray-300" ] ] []

        progress =
            case model.session.test of
                Just test ->
                    List.map viewComplete test.completed ++ [ viewCurrent ] ++ List.map viewFuture test.future

                Nothing ->
                    case state of
                        Unanswered ->
                            []

                        Answered answeredData ->
                            if answeredData.choice.isCorrect then
                                [ span [ tailwind [ "text-green-500" ] ] [ text "Well done!" ] ]

                            else
                                [ span [ tailwind [ "text-red-500" ] ] [ text "Better luck next time!" ] ]
    in
    header
        [ tailwind
            [ "items-center", "flex", "bg-white" ]
        ]
        [ div [ tailwind [ "flex-grow", "flex-wrap", "flex", "overflow-auto" ] ]
            progress
        , button [ onClick ClickedClose, tailwind [ "hover:text-blue-800", "focus:text-blue-200" ] ]
            [ i [ class "material-icons" ] [ text "close" ] ]
        ]


viewFooter : Model -> State -> Question.ReadData -> Html Msg
viewFooter model state question =
    let
        ( isCorrect, isIncorrect ) =
            case state of
                Unanswered ->
                    ( False, False )

                Answered answeredData ->
                    if answeredData.choice.isCorrect then
                        ( True, False )

                    else
                        ( False, True )

        footerButtonText =
            case model.session.test of
                Just test ->
                    case test.future of
                        [] ->
                            "Review Results"

                        head :: tail ->
                            "Next Question"

                Nothing ->
                    "Go Home"

        numLikesInfo =
            case question.numLikes of
                Just int ->
                    div [ tailwind [ "ml-2", "font-bold", "text-sm" ] ] [ text (String.fromInt int) ]

                Nothing ->
                    div [] []

        ( likeStyles, canLike ) =
            case question.liked of
                Just False ->
                    ( "border-2 border-blue-500 hover:bg-blue-500 hover:text-white", True )

                _ ->
                    ( "", False )

        ( isFlagDisabled, flagContents ) =
            case state of
                Unanswered ->
                    ( False, span [] [] )

                Answered answeredData ->
                    case answeredData.flagResponse of
                        NotAsked ->
                            ( False, span [ class "material-icons" ] [ text "flag" ] )

                        Loading ->
                            ( True, span [ class "material-icons" ] [ text "hourglass_empty" ] )

                        Success _ ->
                            ( True, span [] [ text "Received" ] )

                        Failure _ ->
                            ( False, span [] [ text "Error - try again later." ] )
    in
    footer
        [ classList
            [ ( "opacity-0", state == Unanswered )
            , ( "correct-bg", isCorrect )
            , ( "incorrect-bg", isIncorrect )
            ]
        , tailwind
            [ "transition", "opacity-1" ]
        ]
        [ button
            [ onClick ClickedFlag
            , tailwind [ "mx-1" ]
            , classList
                [ ( "border-2 border-blue-500 hover:bg-blue-500 hover:text-white", not isFlagDisabled )
                , ( "text-gray-600 cursor-auto uppercase text-bold", isFlagDisabled )
                ]
            , type_ "button"
            , classList [ ( "hidden", Session.isGuest model.session ) ]
            , disabled isFlagDisabled
            ]
            [ flagContents ]
        , button
            [ onClick ClickedLike
            , tailwind [ "mx-1", "flex", "items-center" ]
            , class likeStyles
            , disabled (not canLike)
            , type_ "button"
            , classList
                [ ( "cursor-auto text-gray-600", Session.isGuest model.session || Maybe.withDefault True question.liked )
                , ( "border-blue-500", not <| Session.isGuest model.session && not (Maybe.withDefault True question.liked) )
                ]
            ]
            [ span [ class "material-icons" ] [ text "thumb_up" ]
            , numLikesInfo
            ]
        , button
            [ onClick ClickedNextQuestion
            , tailwind
                [ "mx-1"
                , "border-2"
                , "border-blue-500"
                , "uppercase"
                , "text-sm"
                , "font-bold"
                , "hover:bg-blue-500"
                , "hover:text-white"
                ]
            , type_ "button"
            ]
            [ text footerButtonText ]
        ]


viewChoiceRead : Int -> State -> Choice.ReadData -> Html Msg
viewChoiceRead total state choice =
    let
        percentChosen =
            case total of
                0 ->
                    0

                _ ->
                    toFloat choice.numChosen
                        / toFloat total
                        |> (*) 100
                        |> round
    in
    case state of
        Unanswered ->
            button
                [ class "choice"
                , onClick (ClickedChoice choice)
                , tailwind
                    [ "my-1"
                    , "flex"
                    , "justify-start"
                    , "border-2"
                    , "border-blue-500"
                    , "hover:bg-blue-500"
                    , "text-blue-500"
                    , "font-semibold"
                    , "hover:text-white"
                    , "p-2"
                    ]
                ]
                [ span [] [ text choice.content ] ]

        Answered answeredData ->
            let
                opened =
                    answeredData.choice.id == choice.id

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

                --, Html.Attributes.attribute openedString ""
                ]
                [ summary
                    [ class "choice"
                    , classList
                        [ ( "bg-red-500 text-white border-2 border-red-500", not choice.isCorrect && opened )
                        , ( "bg-green-500 text-white border-2 border-green-500", choice.isCorrect )
                        , ( "bg-gray-300 text-gray-700 border-2 border-gray-300", not choice.isCorrect && not opened )
                        ]
                    , tailwind
                        [ "flex"
                        , "p-2"
                        , "rounded"
                        , "items-center"
                        , "font-semibold"
                        ]
                    ]
                    [ span [] [ text choice.content ]
                    , span [ tailwind [ "ml-auto", "font-normal" ] ] [ text (String.fromInt percentChosen ++ "%") ]
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


viewQuestionComments : Model -> State -> Question.ReadData -> Html Msg
viewQuestionComments model state question =
    case state of
        Unanswered ->
            article
                [ id "comments" ]
                []

        Answered answeredData ->
            let
                backgroundColor =
                    case answeredData.choice.isCorrect of
                        True ->
                            "correct-bg"

                        False ->
                            "incorrect-bg"

                submitText =
                    case answeredData.commentResponse of
                        Loading ->
                            "Loading"

                        _ ->
                            "Submit"
            in
            article [ id "comments" ]
                [ section []
                    [ div [ id "comments" ]
                        (List.map viewCommentChatStyle question.comments)
                    ]
                , footer
                    [ tailwind
                        [ backgroundColor, "flex", "flex-col" ]
                    ]
                    [ textarea
                        [ placeholder "Comment here."
                        , value answeredData.comment
                        , onInput ChangedComment
                        , tailwind [ "text-black", "text-sm" ]
                        , classList [ ( "hidden", Session.isGuest model.session ) ]
                        , required True
                        , rows 4
                        ]
                        []
                    , button
                        [ onClick ClickedSubmitComment
                        , tailwind
                            [ "mt-1"
                            , "border-2"
                            , "border-blue-500"
                            , "hover:bg-blue-500"
                            , "hover:text-white"
                            , "uppercase"
                            , "text-sm"
                            , "font-bold"
                            ]
                        , classList [ ( "hidden", Session.isGuest model.session ) ]
                        ]
                        [ text submitText ]
                    ]
                ]
