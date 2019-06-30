module Page.Note exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (getAt, setAt)
import Markdown
import Random
import Random.List exposing (choose)
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
import Types.Specialty as Specialty exposing (Specialty)
import Types.Styles exposing (tailwind)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Views.Question exposing (..)



-- Model


type alias Model =
    { session : Session
    , noteId : Int
    , webDataNote : WebData Note.Data
    , comment : String
    , webDataComment : WebData Comment.ReadData
    , modal : Modal
    , tab : Tab
    }


type Modal
    = ModalNone
    | ModalAddQuestion AddQuestionData


type Tab
    = Official
    | Community


type alias AddQuestionData =
    { question : Question.CreationData
    , response : WebData Question.ReadData
    }



-- Msg


type Msg
    = NoOp
    | GotNote (WebData Note.Data)
    | ClickedTab Tab
    | OpenedAddQuestionModal
    | OpenedStudyModal
    | GotRandomQuestionId ( Maybe Question.ListData, List Question.ListData )
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
      , tab = Official
      }
    , Request.get (getNote session noteId)
    )


initAddQuestion : AddQuestionData
initAddQuestion =
    { question = Question.new
    , response = NotAsked
    }


initStudy : Int -> ModalQuestionData
initStudy questionId =
    { questionId = questionId
    , webData = Loading
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

        ( ClickedTab tab, _ ) ->
            ( { model | tab = tab }, Cmd.none )

        ( OpenedAddQuestionModal, _ ) ->
            ( { model | modal = ModalAddQuestion initAddQuestion }, Cmd.none )

        ( OpenedStudyModal, Success noteData ) ->
            case ( model.session.auth, noteData.dueIds, noteData.knownIds ) of
                ( User _, Just [], Just known ) ->
                    -- If there are no due questions, select randomly from all cards
                    ( model, Random.generate GotRandomQuestionId (choose noteData.allIds) )

                ( User _, Just due, _ ) ->
                    -- Or do the due questions first
                    ( model, Random.generate GotRandomQuestionId (choose due) )

                _ ->
                    ( model, Random.generate GotRandomQuestionId (choose noteData.allIds) )

        ( GotRandomQuestionId ( maybeQuestionListData, left ), Success noteData ) ->
            case maybeQuestionListData of
                Just ({ id } as selected) ->
                    -- Modify the question sets
                    let
                        newAllIds =
                            List.Extra.remove selected noteData.allIds

                        newDue =
                            case ( model.session.auth, noteData.dueIds ) of
                                ( User _, Just due ) ->
                                    Just (List.Extra.remove selected due)

                                _ ->
                                    Nothing
                    in
                    ignore

                Nothing ->
                    -- Reload
                    let
                        newSession =
                            Session.addMessage
                                model.session
                                "There aren't any questions attached to this note yet. You can add some if you are logged in!"
                    in
                    ( { model | session = newSession, modal = ModalNone }
                    , Request.get (getNote model.session model.noteId)
                    )

        ( ClickedCloseModal, _ ) ->
            -- Reload
            ( { model | modal = ModalNone, webDataNote = Loading }, Request.get (getNote model.session model.noteId) )

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
    { title = "AORTA - Note"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_
        [ tailwind
            [ "h-screen"
            , "bg-grey-200"
            , "container"
            , "mx-auto"
            , "overflow-hidden"
            , "p-4"
            , "pt-10"
            ]
        ]
        [ section
            [ tailwind [ "flex" ] ]
            [ viewHeader model.webDataNote
            , section
                [ tailwind
                    [ "w-3/4"
                    , "overflow-auto"
                    , "md:overflow-hidden"
                    , "md:flex"
                    , "md:justify-center"
                    , "md:items-center"
                    , "p-2"
                    ]
                ]
                [ viewContent model model.webDataNote
                ]
            ]
        ]
    , viewModal model.modal
    ]


viewHeader : WebData Note.Data -> Html Msg
viewHeader dataNoteWebData =
    let
        headerColor =
            case dataNoteWebData of
                Success data ->
                    data.specialty |> Specialty.toMedium |> Color.toCssString

                _ ->
                    "slategray"

        wrap title yearLevel specialty allIds loading =
            section
                [ tailwind
                    [ "flex"
                    , "flex-col"
                    , "items-center"
                    , "transition"
                    , "w-1/4"
                    , "p-4"
                    , "mt-4"
                    ]
                ]
                [ div
                    [ Html.Attributes.style "background" headerColor
                    , tailwind
                        [ "w-56"
                        , "h-56"
                        , "rounded-lg"
                        , "p-4"
                        , "transition"
                        ]
                    ]
                    [ div
                        [ tailwind
                            [ "text-lg", "text-white" ]
                        ]
                        [ text title ]
                    , div
                        [ tailwind
                            [ "ml-auto"
                            , "sm:flex"
                            , "mr-2"
                            , "hidden"
                            ]
                        ]
                        [ div [ class "tag" ] [ text yearLevel ]
                        , div [ class "tag" ] [ text specialty ]
                        ]
                    ]
                , a
                    [ href "/"
                    , tailwind
                        [ "mr-4"
                        , "flex"
                        , "items-center"
                        , "p-2"
                        , "px-4"
                        , "hover:bg-white"
                        , "hover:text-black"
                        ]
                    ]
                    [ i [ class "material-icons" ] [ text "arrow_back" ]
                    , span [ tailwind [ "ml-2" ] ] [ text "Back" ]
                    ]
                , div [ tailwind [ "ml-2" ] ]
                    [ text <| String.fromInt (List.length allIds) ++ " attached EMQs." ]
                , button
                    [ onClick OpenedAddQuestionModal
                    , tailwind
                        [ "mx-2"
                        ]
                    ]
                    [ text "Add EMQ" ]
                , button
                    [ onClick OpenedStudyModal
                    , tailwind
                        [ "mx-2"
                        ]
                    ]
                    [ text "Study" ]
                ]
    in
    case dataNoteWebData of
        Loading ->
            wrap
                "Loading"
                ""
                ""
                []
                True

        NotAsked ->
            wrap
                "Not Asked"
                ""
                ""
                []
                False

        Failure e ->
            wrap
                "Failure"
                ""
                ""
                []
                False

        Success data ->
            wrap
                data.title
                (YearLevel.toString data.yearLevel)
                (Specialty.toString data.specialty)
                data.allIds
                False


viewContent : Model -> WebData Note.Data -> Html Msg
viewContent model dataNoteWebData =
    let
        wrap content =
            div
                [ class "markdown"
                , tailwind
                    [ "container"
                    ]
                ]
                content
    in
    case dataNoteWebData of
        Loading ->
            wrap [ div [ class "loading" ] [] ]

        NotAsked ->
            wrap [ text "Not asked" ]

        Failure e ->
            wrap [ text "Failure" ]

        Success data ->
            let
                dataContent =
                    case model.tab of
                        Official ->
                            viewNote data.content

                        Community ->
                            [ div
                                [ id "comments"
                                , tailwind
                                    [ "md:flex-grow"
                                    , "md:overflow-auto"
                                    ]
                                ]
                                (List.map viewComment data.comments)
                            , div
                                [ tailwind [ "mt-2" ] ]
                                [ footer
                                    [ tailwind [] ]
                                    [ textarea
                                        [ placeholder "Contribute comments, mnemonics or extra notes here."
                                        , value model.comment
                                        , required True
                                        , onInput ChangedComment
                                        ]
                                        []
                                    , button
                                        [ onClick ClickedSubmitComment
                                        , tailwind [ "float-right" ]
                                        ]
                                        [ text "Submit" ]
                                    ]
                                ]
                            ]
            in
            wrap
                [ button [ onClick (ClickedTab Official) ] [ text "Starter Notes" ]
                , button [ onClick (ClickedTab Community) ] [ text "Contributions" ]
                , article
                    [ id "note"
                    , tailwind
                        [ "bg-white"
                        , "shadow-lg"
                        , "rounded"
                        , "m-1"
                        , "md:m-2"
                        , "p-4"
                        , "md:h-85vh"
                        , "md:overflow-auto"
                        ]
                    ]
                    dataContent
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


viewModal : Modal -> Html Msg
viewModal modal =
    case modal of
        ModalNone ->
            div [] []

        ModalAddQuestion addQuestionData ->
            viewModalAddQuestion addQuestionData


viewModalAddQuestion : AddQuestionData -> Html Msg
viewModalAddQuestion addQuestionData =
    section [ class "modal" ]
        [ Html.form [ onSubmit (AddQuestionMsg PostedQuestion) ]
            [ article []
                [ header
                    [ tailwind
                        [ "flex", "items-center" ]
                    ]
                    [ h1 []
                        [ text "Add EMQ" ]
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
                            (List.map (Domain.option addQuestionData.question.domain) Domain.list)
                        ]
                    , div []
                        (List.indexedMap
                            viewCreateChoice
                            addQuestionData.question.choices
                        )
                        |> Html.map AddQuestionMsg
                    , div []
                        [ button
                            [ onClick (AddQuestionMsg AddedChoice), type_ "button" ]
                            [ text "Add Distractor" ]
                        ]
                    ]
                , footer []
                    [ button [ type_ "submit" ] [ text "Add Question" ]
                    ]
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
                [ label
                    [ for choiceId
                    ]
                    [ text "Answer" ]
                , input
                    [ value creationDataChoice.content
                    , placeholder "Correct choice"
                    , onInput (ChangedChoiceContent int)
                    , id choiceId
                    , class "correct"
                    , required True
                    , tailwind [ "mb-1" ]
                    ]
                    []
                , textarea
                    [ value creationDataChoice.explanation
                    , placeholder "This is correct because..."
                    , onInput (ChangedChoiceExplanation int)
                    ]
                    []
                ]

        val ->
            div [ class "field choice" ]
                [ label
                    [ for choiceId
                    ]
                    [ text "Distractor" ]
                , input
                    [ value creationDataChoice.content
                    , placeholder "Incorrect choice"
                    , onInput (ChangedChoiceContent int)
                    , id choiceId
                    , class "incorrect"
                    , required True
                    , tailwind [ "mb-1" ]
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
