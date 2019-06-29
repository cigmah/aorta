module Page.Revise exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Domain as Domain exposing (Domain)
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.Styles exposing (tailwind)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Url.Builder as Builder
import Views.Question exposing (..)



-- Model


type alias Model =
    { session : Session
    , response : WebData Question.ReadData
    , modal : Modal
    }


type Modal
    = ModalNone
    | ModalQuestion ModalQuestionData



-- Msg


type Msg
    = NoOp
    | ChangedYearLevel String
    | ChangedSpecialty String
    | ClickedStart
    | ClickedFinish
    | GotQuestion (WebData Question.ReadData)
    | StudyMsg StudySubMsg


type StudySubMsg
    = ClickedChoice Choice.ReadData
    | GotResponseResponse (WebData ())
    | ClickedLike
    | GotLikeResponse (WebData ())
    | ClickedFlag
    | GotFlagResponse (WebData ())
    | ChangedQuestionComment String
    | ClickedSubmitQuestionComment
    | GotSubmitQuestionCommentResponse (WebData Comment.ReadData)



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , response = NotAsked
      , modal = ModalNone
      }
    , Cmd.none
    )


initQuestionModal : Question.ReadData -> ModalQuestionData
initQuestionModal data =
    { questionId = data.id
    , webData = Success data
    , state = Unanswered
    , comment = ""
    , commentResponse = NotAsked
    , likeResponse = NotAsked
    , flagResponse = NotAsked
    }



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
update msg ({ session } as model) =
    let
        ignore =
            ( model, Cmd.none )

        unwrap intString =
            String.toInt intString
                |> Maybe.withDefault 0
    in
    case msg of
        NoOp ->
            ignore

        ChangedYearLevel string ->
            let
                newSession =
                    { session | reviseYearLevel = string |> unwrap |> YearLevel.fromInt }
            in
            ( { model | session = newSession }, Session.save newSession )

        ChangedSpecialty string ->
            let
                newSession =
                    { session | reviseSpecialty = string |> unwrap |> Specialty.fromInt }
            in
            ( { model | session = newSession }, Session.save newSession )

        ClickedStart ->
            case model.response of
                Loading ->
                    ignore

                -- TODO incorporate filters
                _ ->
                    ( { model | response = Loading }, Request.get (getRandomQuestion model) )

        ClickedFinish ->
            ( { model | response = NotAsked, modal = ModalNone }, Cmd.none )

        GotQuestion webData ->
            case webData of
                Success data ->
                    ( { model
                        | response = webData
                        , modal = ModalQuestion (initQuestionModal data)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | response = webData }, Cmd.none )

        StudyMsg subMsg ->
            case model.modal of
                ModalQuestion modalData ->
                    updateStudy subMsg modalData model

                _ ->
                    ignore


updateStudy : StudySubMsg -> ModalQuestionData -> Model -> ( Model, Cmd Msg )
updateStudy msg questionData model =
    let
        wrap newQuestion =
            { model | modal = ModalQuestion newQuestion }

        ignore =
            ( model, Cmd.none )
    in
    case msg of
        ClickedChoice choice ->
            case model.session.auth of
                Guest ->
                    ( wrap { questionData | state = Answered choice NotAsked }
                    , Cmd.none
                    )

                User _ ->
                    case questionData.state of
                        Unanswered ->
                            ( wrap { questionData | state = Answered choice Loading }
                            , Request.post (postResponse model.session questionData.questionId choice.id) |> Cmd.map StudyMsg
                            )

                        Answered _ _ ->
                            ignore

        GotResponseResponse webData ->
            case questionData.state of
                Unanswered ->
                    ignore

                Answered choice _ ->
                    ( wrap { questionData | state = Answered choice webData }
                    , Cmd.none
                    )

        ClickedLike ->
            case questionData.likeResponse of
                Loading ->
                    ignore

                _ ->
                    ( wrap { questionData | likeResponse = Loading }
                    , Request.post (postLike model.session questionData.questionId)
                        |> Cmd.map StudyMsg
                    )

        GotLikeResponse webData ->
            ( wrap { questionData | likeResponse = webData }
            , Cmd.none
            )

        ClickedFlag ->
            case questionData.flagResponse of
                Loading ->
                    ignore

                _ ->
                    ( wrap { questionData | flagResponse = Loading }
                    , Request.post (postFlag model.session questionData.questionId)
                        |> Cmd.map StudyMsg
                    )

        GotFlagResponse webData ->
            ( wrap { questionData | flagResponse = webData }
            , Cmd.none
            )

        ChangedQuestionComment string ->
            ( wrap { questionData | comment = string }, Cmd.none )

        ClickedSubmitQuestionComment ->
            case questionData.commentResponse of
                Loading ->
                    ignore

                _ ->
                    ( wrap { questionData | commentResponse = Loading }
                    , Request.post (postQuestionComment model.session questionData.questionId questionData.comment)
                        |> Cmd.map StudyMsg
                    )

        GotSubmitQuestionCommentResponse webData ->
            case webData of
                Success commentReturn ->
                    case questionData.webData of
                        Success questionWebData ->
                            ( wrap
                                { questionData
                                    | commentResponse = webData
                                    , comment = ""
                                    , webData =
                                        Success
                                            { questionWebData
                                                | comments = questionWebData.comments ++ [ commentReturn ]
                                            }
                                }
                            , Cmd.none
                            )

                        _ ->
                            ignore

                _ ->
                    ( wrap { questionData | commentResponse = webData }, Cmd.none )



-- Requests


getRandomQuestion : Model -> Request.GetRequest Question.ReadData Msg
getRandomQuestion model =
    { endpoint = Request.GetRandomQuestion
    , auth = model.session.auth
    , callback = GotQuestion
    , returnDecoder = Question.decoder
    , queryList =
        [ Builder.int "note__year_level" (YearLevel.toInt model.session.reviseYearLevel)
        , Builder.int "note__specialty" (Specialty.toInt model.session.reviseSpecialty)
        ]
    }


postResponse : Session -> Int -> Int -> Request.PostRequest () StudySubMsg
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


postLike : Session -> Int -> Request.PostRequest () StudySubMsg
postLike session questionId =
    { auth = session.auth
    , endpoint = Request.PostFlag
    , callback = GotLikeResponse
    , returnDecoder = Decode.succeed ()
    , queryList = []
    , body =
        Encode.object
            [ ( "question", Encode.int questionId ) ]
    }


postFlag : Session -> Int -> Request.PostRequest () StudySubMsg
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


postQuestionComment : Session -> Int -> String -> Request.PostRequest Comment.ReadData StudySubMsg
postQuestionComment session questionId comment =
    { auth = session.auth
    , endpoint = Request.PostQuestionComment
    , callback = GotSubmitQuestionCommentResponse
    , returnDecoder = Comment.decoder
    , queryList = []
    , body =
        Encode.object
            [ ( "question", Encode.int questionId )
            , ( "content", Encode.string comment )
            ]
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Revise"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    let
        yearLevelSelect =
            select
                [ onInput <| ChangedYearLevel
                , value (model.session.reviseYearLevel |> YearLevel.toInt |> String.fromInt)
                ]
                (List.map (YearLevel.option model.session.reviseYearLevel) YearLevel.list)

        specialtySelect =
            select
                [ onInput <| ChangedSpecialty
                , value (model.session.reviseSpecialty |> Specialty.toInt |> String.fromInt)
                ]
                (List.map (Specialty.option model.session.reviseSpecialty) Specialty.list)
    in
    [ main_
        [ tailwind
            [ "min-h-screen"
            , "flex"
            , "justify-center"
            , "items-center"
            , "w-full"
            , "overflow-auto"
            ]
        ]
        [ section
            [ tailwind [ "container", "flex", "justify-center", "items-center" ] ]
            [ article
                [ tailwind
                    [ "bg-white"
                    , "shadow"
                    , "rounded"
                    , "flex"
                    , "flex-col"
                    , "h-full"
                    ]
                ]
                [ header
                    [ tailwind
                        [ "p-2"
                        , "bg-blue-800"
                        , "text-white"
                        ]
                    ]
                    [ text "Revise Random Questions" ]
                , section
                    [ tailwind
                        [ "p-4"
                        , "flex-grow"
                        ]
                    ]
                    [ div [ class "field" ]
                        [ label [] [ text "Year Level" ]
                        , yearLevelSelect
                        ]
                    , div [ class "field" ]
                        [ label [] [ text "Specialty" ]
                        , specialtySelect
                        ]
                    ]
                , footer
                    [ tailwind
                        [ "px-2"
                        , "py-1"
                        , "bg-blue-800"
                        , "text-white"
                        , "flex"
                        , "justify-end"
                        ]
                    ]
                    [ button [ onClick ClickedStart ] [ text "Start" ] ]
                ]
            ]
        ]
    , viewModal model
    ]


questionMsgs : QuestionMsgs Msg
questionMsgs =
    { clickedLike = StudyMsg ClickedLike
    , clickedFlag = StudyMsg ClickedFlag
    , nextQuestion = ClickedStart
    , clickedChoice = StudyMsg << ClickedChoice
    , changedComment = StudyMsg << ChangedQuestionComment
    , submitComment = StudyMsg ClickedSubmitQuestionComment
    , clickedClose = ClickedFinish
    }


viewModal : Model -> Html Msg
viewModal model =
    case model.modal of
        ModalNone ->
            div [] []

        ModalQuestion modalData ->
            case modalData.webData of
                Success question ->
                    viewQuestionSection questionMsgs modalData question

                Failure e ->
                    div [] []

                Loading ->
                    div [] []

                NotAsked ->
                    div [] []
