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
    let
        encoded =
            Question.encode noteId question

        _ =
            Debug.log "encoded" encoded
    in
    { auth = session.auth
    , endpoint = Request.PostQuestion
    , callback = AddQuestionMsg << GotAddQuestionResponse
    , returnDecoder = Question.decoder
    , queryList = []
    , body = encoded
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
            [ viewHeader model.webDataNote ]
        , section []
            [ div [ id "dashboard" ]
                [ viewStats model.webDataNote
                , viewContent model.webDataNote
                , viewControls model.webDataNote
                ]
            ]
        ]
    , viewModal model.modal
    ]


viewHeader : WebData Note.Data -> Html Msg
viewHeader dataNoteWebData =
    let
        wrap string =
            div []
                [ a [ href "/" ] [ text "Back" ]
                , text string
                ]
    in
    case dataNoteWebData of
        Loading ->
            wrap "Loading"

        NotAsked ->
            wrap "Not Asked"

        Failure e ->
            wrap "Failure"

        Success data ->
            wrap data.title


viewContent : WebData Note.Data -> Html Msg
viewContent dataNoteWebData =
    case dataNoteWebData of
        Loading ->
            article [] [ text "Loading" ]

        NotAsked ->
            article [] [ text "Not asked" ]

        Failure e ->
            article [] [ text "Failure" ]

        Success data ->
            article [ id "content" ]
                [ section [] (Markdown.toHtml Nothing data.content)
                , footer [] []
                ]


viewStats : WebData Note.Data -> Html Msg
viewStats dataNoteWebData =
    case dataNoteWebData of
        Loading ->
            article [] [ text "Loading" ]

        NotAsked ->
            article [] [ text "Not asked" ]

        Failure e ->
            article [] [ text "Failure" ]

        Success data ->
            article [ id "stats" ] []


viewControls : WebData Note.Data -> Html Msg
viewControls dataNoteWebData =
    case dataNoteWebData of
        Loading ->
            article [] [ text "Loading" ]

        NotAsked ->
            article [] [ text "Not asked" ]

        Failure e ->
            article [] [ text "Failure" ]

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
                |> Html.map AddQuestionMsg

        ModalQuestion modalQuestionData ->
            section [] []


viewModalAddQuestion : AddQuestionData -> Html AddQuestionSubMsg
viewModalAddQuestion addQuestionData =
    section [ id "modal" ]
        [ article []
            [ header [] [ text "Add EMQ" ]
            , section []
                [ textarea
                    [ value addQuestionData.question.stem
                    , placeholder "Question stem"
                    , onInput ChangedStem
                    ]
                    []
                , select
                    [ onInput ChangedDomain
                    , value (addQuestionData.question.domain |> Domain.toInt |> String.fromInt)
                    ]
                    (List.map Domain.option Domain.list)
                , div []
                    (List.indexedMap
                        viewCreateChoice
                        addQuestionData.question.choices
                    )
                , button [ onClick AddedChoice ] [ text "Add Choice" ]
                , button [ onClick PostedQuestion ] [ text "Add Question" ]
                ]
            ]
        ]


viewCreateChoice : Int -> Choice.CreationData -> Html AddQuestionSubMsg
viewCreateChoice int creationDataChoice =
    case int of
        0 ->
            div []
                [ input
                    [ value creationDataChoice.content
                    , placeholder "Correct choice"
                    , onInput (ChangedChoiceContent int)
                    ]
                    []
                , input
                    [ value creationDataChoice.explanation
                    , placeholder "This is correct because..."
                    , onInput (ChangedChoiceExplanation int)
                    ]
                    []
                ]

        val ->
            div []
                [ input
                    [ value creationDataChoice.content
                    , placeholder "Incorrect choice"
                    , onInput (ChangedChoiceContent int)
                    ]
                    []
                , input
                    [ value creationDataChoice.explanation
                    , placeholder "This is incorrect because..."
                    , onInput (ChangedChoiceExplanation int)
                    ]
                    []
                ]
