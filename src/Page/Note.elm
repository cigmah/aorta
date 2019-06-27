module Page.Note exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (getAt, setAt)
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Datetime as Datetime
import Types.Domain as Domain exposing (Domain)
import Types.Note as Note
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)



-- Model


type alias Model =
    { session : Session
    , noteId : Int
    , webDataNote : WebData Note.Data
    , comment : String
    , webDataComment : WebData Comment.ReadData
    , modal : Modal
    }


type Modal
    = ModalNone
    | ModalAddQuestion AddQuestionData
    | ModalQuestion ModalQuestionData


type alias AddQuestionData =
    { question : Question.CreationData
    , response : WebData Question.ReadData
    }


type alias ModalQuestionData =
    { questionId : Int
    , webData : WebData Question.ReadData
    , state : QuestionState
    }


type QuestionState
    = Unanswered
    | Answered Int (WebData ())



-- Msg


type Msg
    = NoOp
    | GotNote (WebData Note.Data)
    | OpenedAddQuestionModal
    | ClickedCloseModal
    | ChangedComment String
    | ClickedSubmitComment
    | GotSubmitCommentResponse (WebData Comment.ReadData)
    | AddQuestionMsg AddQuestionSubMsg


type AddQuestionSubMsg
    = ChangedStem String
    | ChangedDomain String
    | ChangedChoiceContent Int String
    | ChangedChoiceExplanation Int String
    | AddedChoice
    | PostedQuestion
    | GotAddQuestionResponse (WebData Question.ReadData)



-- Init


init : Session -> Int -> ( Model, Cmd Msg )
init session noteId =
    ( { session = session
      , noteId = noteId
      , webDataNote = Loading
      , comment = ""
      , webDataComment = NotAsked
      , modal = ModalNone
      }
    , Request.get (getNote session noteId)
    )


initAddQuestion : AddQuestionData
initAddQuestion =
    { question = Question.new
    , response = NotAsked
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
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case ( msg, model.webDataNote ) of
        ( NoOp, _ ) ->
            ignore

        ( GotNote webData, Loading ) ->
            ( { model | webDataNote = webData }, Cmd.none )

        ( OpenedAddQuestionModal, _ ) ->
            ( { model | modal = ModalAddQuestion initAddQuestion }, Cmd.none )

        ( ClickedCloseModal, _ ) ->
            ( { model | modal = ModalNone }, Cmd.none )

        ( ChangedComment string, Success noteData ) ->
            ( { model | comment = string }, Cmd.none )

        ( ClickedSubmitComment, Success noteData ) ->
            case model.webDataComment of
                Loading ->
                    ignore

                _ ->
                    ( { model | webDataComment = Loading }
                    , Request.post (postComment model.session model.noteId model.comment)
                    )

        ( GotSubmitCommentResponse webData, Success noteData ) ->
            case webData of
                Success comment ->
                    -- Easy route for now.
                    ( { model
                        | webDataComment = webData
                        , comment = ""
                        , webDataNote = Success { noteData | comments = noteData.comments ++ [ comment ] }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | webDataComment = webData }, Cmd.none )

        ( AddQuestionMsg subMsg, Success noteData ) ->
            case model.modal of
                ModalAddQuestion modalData ->
                    updateAddQuestion subMsg modalData noteData model

                _ ->
                    ignore

        _ ->
            ignore


updateAddQuestion : AddQuestionSubMsg -> AddQuestionData -> Note.Data -> Model -> ( Model, Cmd Msg )
updateAddQuestion subMsg modalData noteData model =
    let
        question =
            modalData.question

        updateQuestion newQuestion =
            { model | modal = ModalAddQuestion { modalData | question = newQuestion } }

        ignore =
            ( model, Cmd.none )
    in
    case subMsg of
        ChangedStem string ->
            ( updateQuestion { question | stem = string }, Cmd.none )

        ChangedDomain string ->
            let
                newDomain =
                    string
                        |> String.toInt
                        |> Maybe.withDefault 0
                        |> Domain.fromInt
            in
            ( updateQuestion { question | domain = newDomain }, Cmd.none )

        ChangedChoiceContent int string ->
            let
                oldChoice =
                    getAt int question.choices

                newChoices =
                    case oldChoice of
                        Just choice ->
                            setAt int { choice | content = string } question.choices

                        Nothing ->
                            question.choices
            in
            ( updateQuestion { question | choices = newChoices }, Cmd.none )

        ChangedChoiceExplanation int string ->
            let
                oldChoice =
                    getAt int question.choices

                newChoices =
                    case oldChoice of
                        Just choice ->
                            setAt int { choice | explanation = string } question.choices

                        Nothing ->
                            question.choices
            in
            ( updateQuestion { question | choices = newChoices }, Cmd.none )

        AddedChoice ->
            ( updateQuestion { question | choices = question.choices ++ [ Choice.newIncorrect ] }
            , Cmd.none
            )

        PostedQuestion ->
            case modalData.response of
                Loading ->
                    ignore

                _ ->
                    -- TODO Validate
                    ( { model | modal = ModalAddQuestion { modalData | response = Loading } }
                    , Request.post (postQuestion model.session noteData.id modalData.question)
                    )

        GotAddQuestionResponse webData ->
            case webData of
                Success _ ->
                    let
                        newSession =
                            Session.addMessage model.session "Thank you! Your EMQ has been added to this topic."
                    in
                    ( { model
                        | modal = ModalAddQuestion { modalData | response = webData }
                        , session = newSession
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | modal = ModalAddQuestion { modalData | response = webData } }, Cmd.none )



-- Requests


getNote : Session -> Int -> Request.GetRequest Note.Data Msg
getNote session noteId =
    { auth = session.auth
    , endpoint = Request.GetNote noteId
    , callback = GotNote
    , returnDecoder = Note.decoder
    , queryList = []
    }


postQuestion : Session -> Int -> Question.CreationData -> Request.PostRequest Question.ReadData Msg
postQuestion session noteId question =
    { auth = session.auth
    , endpoint = Request.PostQuestion
    , callback = AddQuestionMsg << GotAddQuestionResponse
    , returnDecoder = Question.decoder
    , queryList = []
    , body = Question.encode noteId question
    }


postComment : Session -> Int -> String -> Request.PostRequest Comment.ReadData Msg
postComment session int comment =
    { auth = session.auth
    , endpoint = Request.PostComment
    , callback = GotSubmitCommentResponse
    , returnDecoder = Comment.decoder
    , queryList = []
    , body =
        Comment.encode
            { noteId = int
            , content = comment
            }
    }



-- View


view : Model -> Document Msg
view model =
    { title = ""
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_
        [ id "note" ]
        [ header []
            (viewHeader model.webDataNote)
        , section []
            [ div [ id "dashboard" ]
                [ viewStats model.webDataNote
                , viewContent model model.webDataNote
                , viewControls model.webDataNote
                ]
            ]
        ]
    , viewModal model.modal
    ]


viewHeader : WebData Note.Data -> List (Html Msg)
viewHeader dataNoteWebData =
    let
        wrap string loading =
            [ a [ href "/" ] [ text "Back" ]
            , div [ class "title", classList [ ( "fadeinout", loading ) ] ] [ text string ]
            ]
    in
    case dataNoteWebData of
        Loading ->
            wrap "Loading" True

        NotAsked ->
            wrap "Not Asked" False

        Failure e ->
            wrap "Failure" False

        Success data ->
            wrap data.title False


viewContent : Model -> WebData Note.Data -> Html Msg
viewContent model dataNoteWebData =
    case dataNoteWebData of
        Loading ->
            article [ id "content", class "center" ] [ div [ class "loading" ] [] ]

        NotAsked ->
            article [ id "content", class "center" ] [ text "Not asked" ]

        Failure e ->
            article [ id "content", class "center" ] [ text "Failure" ]

        Success data ->
            article [ id "content" ]
                [ section []
                    [ div [ id "note" ]
                        (viewNote data.content)
                    , div [ id "comments" ]
                        (List.map viewComment data.comments)
                    ]
                , footer []
                    [ textarea
                        [ placeholder "Contribute"
                        , value model.comment
                        , required True
                        , onInput ChangedComment
                        ]
                        []
                    , button
                        [ onClick ClickedSubmitComment ]
                        [ text "Submit" ]
                    ]
                ]


viewComment : Comment.ReadData -> Html Msg
viewComment data =
    div [ class "comment" ]
        [ label []
            [ text
                (String.join
                    " "
                    [ data.author.username, "on", Datetime.posixToString data.created_at ]
                )
            ]
        , div [ class "markdown" ]
            (Markdown.toHtml Nothing data.content)
        ]


viewNote : String -> List (Html Msg)
viewNote content =
    case content of
        "" ->
            [ text "We haven't added any official AORTA notes to this item yet - sorry about that! We'll be on it soon. In the meantime, you can submit public contributions below." ]

        value ->
            Markdown.toHtml Nothing content


viewStats : WebData Note.Data -> Html Msg
viewStats dataNoteWebData =
    case dataNoteWebData of
        Loading ->
            article [ id "stats" ] [ div [ class "loading" ] [] ]

        NotAsked ->
            article [ id "stats" ] [ text "Not asked" ]

        Failure e ->
            article [ id "stats" ] [ text "Failure" ]

        Success data ->
            article [ id "stats" ] []


viewControls : WebData Note.Data -> Html Msg
viewControls dataNoteWebData =
    case dataNoteWebData of
        Loading ->
            article [ id "controls" ] [ div [ class "loading" ] [] ]

        NotAsked ->
            article [ id "controls" ] [ text "Not asked" ]

        Failure e ->
            article [ id "controls" ] [ text "Failure" ]

        Success data ->
            article [ id "controls" ]
                [ button [ onClick OpenedAddQuestionModal ] [ text "Add EMQ" ]
                , button [] [ text "Study" ]
                ]


viewModal : Modal -> Html Msg
viewModal modal =
    case modal of
        ModalNone ->
            div [] []

        ModalAddQuestion addQuestionData ->
            viewModalAddQuestion addQuestionData

        ModalQuestion modalQuestionData ->
            section [] []


viewModalAddQuestion : AddQuestionData -> Html Msg
viewModalAddQuestion addQuestionData =
    section [ id "modal" ]
        [ article []
            [ header []
                [ h1 [] [ text "Add EMQ" ]
                , button [ onClick ClickedCloseModal ]
                    [ i [ class "material-icons" ] [ text "close" ] ]
                ]
            , section []
                [ div [ class "field" ]
                    [ label [ for "stem" ] [ text "Question Stem" ]
                    , textarea
                        [ value addQuestionData.question.stem
                        , placeholder "Question stem"
                        , onInput (AddQuestionMsg << ChangedStem)
                        , id "stem"
                        , required True
                        ]
                        []
                    ]
                , div [ class "field" ]
                    [ label [ for "domain" ] [ text "Domain" ]
                    , select
                        [ onInput (AddQuestionMsg << ChangedDomain)
                        , value (addQuestionData.question.domain |> Domain.toInt |> String.fromInt)
                        , id "domain"
                        ]
                        (List.map Domain.option Domain.list)
                    ]
                , div []
                    (List.indexedMap
                        viewCreateChoice
                        addQuestionData.question.choices
                    )
                    |> Html.map AddQuestionMsg
                ]
            , footer []
                [ button [ onClick (AddQuestionMsg AddedChoice) ] [ text "Add Choice" ]
                , button [ onClick (AddQuestionMsg PostedQuestion) ] [ text "Add Question" ]
                ]
            ]
        ]


viewCreateChoice : Int -> Choice.CreationData -> Html AddQuestionSubMsg
viewCreateChoice int creationDataChoice =
    let
        choiceId =
            "choice-" ++ String.fromInt int
    in
    case int of
        0 ->
            div [ class "field choice" ]
                [ label [ for choiceId, class "correct" ] [ text "Answer" ]
                , input
                    [ value creationDataChoice.content
                    , placeholder "Correct choice"
                    , onInput (ChangedChoiceContent int)
                    , id choiceId
                    , class "correct"
                    , required True
                    ]
                    []
                , textarea
                    [ value creationDataChoice.explanation
                    , placeholder "This is correct because..."
                    , onInput (ChangedChoiceExplanation int)
                    , class "correct"
                    ]
                    []
                ]

        val ->
            div [ class "field choice" ]
                [ label [ for choiceId, class "incorrect" ] [ text "Distractor" ]
                , input
                    [ value creationDataChoice.content
                    , placeholder "Incorrect choice"
                    , onInput (ChangedChoiceContent int)
                    , id choiceId
                    , class "incorrect"
                    , required True
                    ]
                    []
                , textarea
                    [ value creationDataChoice.explanation
                    , placeholder "This is incorrect because..."
                    , onInput (ChangedChoiceExplanation int)
                    , class "incorrect"
                    ]
                    []
                ]
