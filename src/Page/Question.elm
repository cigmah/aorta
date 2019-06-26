module Page.Question exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Note as Note
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)



-- Model


type alias Model =
    { session : Session
    , response : WebData Question.ReadData
    , state : State
    }


type State
    = Unanswered
    | Answered AnsweredData


type alias AnsweredData =
    { choiceId : Int
    , responseResponse : WebData ()
    , comment : String
    , commentResponse : WebData Comment.ReadData
    , likeResponse : WebData ()
    , flagResponse : WebData ()
    }


newAnsweredData : Int -> AnsweredData
newAnsweredData int =
    { choiceId = int
    , comment = ""
    , responseResponse = Loading
    , commentResponse = NotAsked
    , likeResponse = NotAsked
    , flagResponse = NotAsked
    }



-- Msg


type Msg
    = NoOp
    | GotQuestionData (WebData Question.ReadData)
    | ClickedChoice Int
    | AnsweredMsg AnsweredSubMsg


type AnsweredSubMsg
    = GotResponseResponse (WebData ())
    | ClickedFlag
    | GotFlagResponse (WebData ())
    | ClickedLike
    | GotLikeResponse (WebData ())
    | ChangedComment String
    | ClickedSubmitComment
    | GotSubmitCommentResponse (WebData Comment.ReadData)



-- Init


init : Session -> Int -> ( Model, Cmd Msg )
init session questionId =
    ( { session = session
      , response = Loading
      , state = Unanswered
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



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            ignore

        GotQuestionData webData ->
            case model.response of
                Loading ->
                    ( { model | response = webData }, Cmd.none )

                _ ->
                    ignore

        ClickedChoice choiceId ->
            case model.response of
                Success question ->
                    ( { model
                        | state =
                            Answered (newAnsweredData choiceId)
                      }
                    , maybeSendResponse model question choiceId
                    )

                _ ->
                    ignore

        AnsweredMsg subMsg ->
            case model.response of
                Success question ->
                    case model.state of
                        Unanswered ->
                            ignore

                        Answered data ->
                            updateAnswered subMsg data question model

                _ ->
                    ignore


updateAnswered : AnsweredSubMsg -> AnsweredData -> Question.ReadData -> Model -> ( Model, Cmd Msg )
updateAnswered msg data question model =
    let
        insert newData =
            { model | state = Answered newData }

        ignore =
            ( model, Cmd.none )
    in
    case msg of
        GotResponseResponse webData ->
            ( insert { data | responseResponse = webData }, Cmd.none )

        ClickedFlag ->
            case data.flagResponse of
                Loading ->
                    ignore

                _ ->
                    ( insert { data | flagResponse = Loading }
                    , Request.post (postFlag model.session question)
                    )

        GotFlagResponse webData ->
            ( insert { data | flagResponse = webData }, Cmd.none )

        ClickedLike ->
            case data.likeResponse of
                Loading ->
                    ignore

                _ ->
                    ( insert { data | likeResponse = Loading }
                    , Request.post (postLike model.session question)
                    )

        GotLikeResponse webData ->
            ( insert { data | likeResponse = webData }, Cmd.none )

        ChangedComment string ->
            ( insert { data | comment = string }, Cmd.none )

        ClickedSubmitComment ->
            case data.commentResponse of
                Loading ->
                    ignore

                _ ->
                    ( insert { data | commentResponse = Loading }
                    , Request.post (postComment model question data.comment)
                    )

        GotSubmitCommentResponse webData ->
            ( insert { data | commentResponse = webData }, Cmd.none )



-- Requests


getQuestion : Session -> Int -> Request.GetRequest Question.ReadData Msg
getQuestion session questionId =
    { endpoint = Request.GetQuestion questionId
    , auth = session.auth
    , callback = GotQuestionData
    , returnDecoder = Question.decoder
    }


postResponse : Session -> Question.ReadData -> Int -> Request.PostRequest () Msg
postResponse session question choiceId =
    { endpoint = Request.PostResponse
    , auth = session.auth
    , callback = AnsweredMsg << GotResponseResponse
    , returnDecoder = Decode.succeed ()
    , body =
        Encode.object
            [ ( "question", Encode.int question.id )
            , ( "choice", Encode.int choiceId )
            ]
    }


postFlag : Session -> Question.ReadData -> Request.PostRequest () Msg
postFlag session question =
    { endpoint = Request.PostFlag
    , auth = session.auth
    , callback = AnsweredMsg << GotFlagResponse
    , returnDecoder = Decode.succeed ()
    , body =
        Encode.object
            [ ( "question", Encode.int question.id )
            ]
    }


postLike : Session -> Question.ReadData -> Request.PostRequest () Msg
postLike session question =
    { endpoint = Request.PostLike
    , auth = session.auth
    , callback = AnsweredMsg << GotLikeResponse
    , returnDecoder = Decode.succeed ()
    , body =
        Encode.object
            [ ( "question", Encode.int question.id ) ]
    }


postComment : Model -> Question.ReadData -> String -> Request.PostRequest Comment.ReadData Msg
postComment model question comment =
    { endpoint = Request.PostComment
    , auth = model.session.auth
    , callback = AnsweredMsg << GotSubmitCommentResponse
    , returnDecoder = Comment.decoder
    , body =
        Comment.encode
            (Comment.CreationData question.note.id comment)
    }


maybeSendResponse : Model -> Question.ReadData -> Int -> Cmd Msg
maybeSendResponse model question choiceId =
    case model.session.auth of
        Guest ->
            Cmd.none

        User _ ->
            Request.post (postResponse model.session question choiceId)



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Question"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_ []
        [ section [] [ viewQuestion model ]
        , section [] [ viewNote model |> Html.map AnsweredMsg ]
        ]
    ]


viewQuestion : Model -> Html Msg
viewQuestion model =
    case model.response of
        Success question ->
            article []
                [ viewQuestionHeader model question
                , viewQuestionBody model question
                , viewQuestionFooter model question |> Html.map AnsweredMsg
                ]

        Loading ->
            text "Loading"

        NotAsked ->
            text "NotAsked"

        Failure e ->
            text "Failure"


viewQuestionHeader : Model -> Question.ReadData -> Html Msg
viewQuestionHeader model question =
    header [] [ text "Question" ]


viewQuestionBody : Model -> Question.ReadData -> Html Msg
viewQuestionBody model question =
    section []
        [ section []
            (Markdown.toHtml Nothing question.stem)
        , section []
            [ ol []
                (List.map (viewChoice model) question.choices)
            ]
        ]


viewChoice : Model -> Choice.ReadData -> Html Msg
viewChoice model choice =
    case model.state of
        Unanswered ->
            li [ onClick (ClickedChoice choice.id) ] [ text choice.content ]

        Answered data ->
            li
                [ classList
                    [ ( "correct", choice.isCorrect )
                    , ( "incorrect", not choice.isCorrect )
                    , ( "selected", choice.id == data.choiceId )
                    ]
                ]
                [ div [] [ text choice.content ]
                , div [] [ text choice.explanation ]
                ]


viewQuestionFooter : Model -> Question.ReadData -> Html AnsweredSubMsg
viewQuestionFooter model question =
    case model.state of
        Unanswered ->
            footer [] []

        Answered choiceId ->
            footer []
                [ text "Good job!"
                , button [ onClick ClickedFlag ] [ text "Flag" ]
                , button [ onClick ClickedLike ] [ text "Like" ]
                , button [] [ text "Next Question" ]
                ]


viewNote : Model -> Html AnsweredSubMsg
viewNote model =
    case model.response of
        Success question ->
            case model.state of
                Unanswered ->
                    text "Unanswered"

                Answered data ->
                    article []
                        [ viewNoteHeader model question data
                        , viewNoteBody model question data
                        , viewNoteFooter model question data
                        ]

        Failure e ->
            text "Failure"

        Loading ->
            text "Loading"

        NotAsked ->
            text "Not asked"


viewNoteHeader : Model -> Question.ReadData -> AnsweredData -> Html AnsweredSubMsg
viewNoteHeader model question data =
    header [] [ text question.note.title ]


viewNoteBody : Model -> Question.ReadData -> AnsweredData -> Html AnsweredSubMsg
viewNoteBody model question data =
    section []
        (Markdown.toHtml Nothing question.note.content)


viewNoteFooter : Model -> Question.ReadData -> AnsweredData -> Html AnsweredSubMsg
viewNoteFooter model question data =
    footer []
        [ Html.form [ onSubmit ClickedSubmitComment ]
            [ textarea
                [ value data.comment
                , onInput ChangedComment
                , placeholder "Write your comment here."
                ]
                []
            , button
                [ type_ "submit"
                ]
                [ text "Post Comment" ]
            ]
        ]
