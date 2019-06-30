module Page.Note exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route
import Browser exposing (Document)
import Browser.Navigation as Navigation
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
    | GotRandomQuestionId ( Maybe Question.ListData, List Question.ListData )
    | ClickedCloseModal
    | ChangedComment String
    | ClickedSubmitComment
    | GotSubmitCommentResponse (WebData Comment.ReadData)
    | ClickedStudy
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

        ( ClickedStudy, Success noteData ) ->
            let
                routeToQuestion listDatum =
                    Navigation.pushUrl
                        session.key
                        (Route.toString (Route.Question listDatum.id))

                sessionWithTest future =
                    { session
                        | test =
                            Just
                                { completed = []
                                , future = List.map .id future
                                , back = Route.toString (Route.Note model.noteId)
                                }
                    }
            in
            case noteData.dueIds of
                -- TODO randomise the choice in which EMQs appear? Maybe not so important for note-based revision though.
                Just (head :: tail) ->
                    ( { model | session = sessionWithTest tail }, routeToQuestion head )

                _ ->
                    case noteData.allIds of
                        head :: tail ->
                            ( { model | session = sessionWithTest tail }, routeToQuestion head )

                        _ ->
                            ( { model | session = Session.addMessage session "There are no EMQs yet!" }, Cmd.none )

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
            , "mx-auto"
            , "overflow-auto"
            , "md:p-4"
            , "md:pt-10"
            ]
        ]
        [ section
            [ tailwind
                [ "md:flex"
                , "container"
                , "mx-auto"
                ]
            ]
            [ viewHeader model.webDataNote
            , section
                [ tailwind
                    [ "md:flex"
                    , "md:justify-center"
                    , "md:p-2"
                    , "mt-2"
                    , "w-full"
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

        makeStudyButton studyButtonText =
            button
                [ onClick ClickedStudy
                , tailwind
                    [ "w-full"
                    , "bg-white"
                    , "text-blue-500"
                    , "rounded"
                    , "p-2"
                    , "items-center"
                    , "border"
                    , "md:border-0"
                    , "flex"
                    , "md:my-1"
                    , "md:shadow"
                    , "hover:bg-blue-400"
                    , "hover:text-white"
                    , "fadein"
                    ]
                ]
                [ i [ class "material-icons" ] [ text "check" ]
                , span [ tailwind [ "ml-2" ] ] [ text studyButtonText ]
                ]

        studyButton =
            case dataNoteWebData of
                Success noteData ->
                    case noteData.dueIds of
                        Just (head :: tail) ->
                            makeStudyButton "Review Due"

                        _ ->
                            case noteData.allIds of
                                [] ->
                                    div [] []

                                list ->
                                    makeStudyButton
                                        ("Study " ++ String.fromInt (List.length list) ++ " EMQs")

                _ ->
                    div [] []

        wrap title yearLevel specialty allIds loading =
            section
                [ tailwind
                    [ "flex"
                    , "flex-col"
                    , "items-center"
                    , "transition"
                    , "md:w-1/4"
                    , "md:p-4"
                    ]
                ]
                [ div
                    [ Html.Attributes.style "background" headerColor
                    , tailwind
                        [ "w-full"
                        , "md:w-40"
                        , "md:h-40"
                        , "lg:w-56"
                        , "lg:h-56"
                        , "md:rounded-lg"
                        , "p-2"
                        , "lg:p-4"
                        , "transition"
                        , "mx-auto"
                        , "md:mb-2"
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
                            , "text-sm"
                            , "lg:text-base"
                            ]
                        ]
                        [ div [ class "tag" ] [ text yearLevel ]
                        , div [ class "tag" ] [ text specialty ]
                        ]
                    ]
                , a
                    [ href "/"
                    , tailwind
                        [ "flex"
                        , "items-center"
                        , "p-2"
                        , "text-blue-500"
                        , "bg-white"
                        , "shadow"
                        , "w-full"
                        , "rounded"
                        , "md:my-1"
                        , "hover:bg-blue-400"
                        , "hover:text-white"
                        ]
                    ]
                    [ i [ class "material-icons" ] [ text "arrow_back" ]
                    , span [ tailwind [ "ml-2" ] ] [ text "Back" ]
                    ]
                , div
                    [ tailwind
                        [ "p-2"
                        , "bg-gray-200"
                        , "text-gray-600"
                        , "w-full"
                        , "md:my-1"
                        , "text-center"
                        , "font-bold"
                        , "text-sm"
                        , "rounded"
                        ]
                    ]
                    [ text <| String.fromInt (List.length allIds) ++ " attached EMQs." ]
                , div
                    [ tailwind [ "flex", "md:block", "w-full" ] ]
                    [ button
                        [ onClick OpenedAddQuestionModal
                        , tailwind
                            [ "w-full"
                            , "bg-white"
                            , "text-blue-500"
                            , "rounded"
                            , "p-2"
                            , "md:shadow"
                            , "border"
                            , "md:border-0"
                            , "items-center"
                            , "flex"
                            , "md:my-1"
                            , "hover:bg-blue-400"
                            , "hover:text-white"
                            ]
                        ]
                        [ i [ class "material-icons" ] [ text "add" ]
                        , span [ tailwind [ "ml-2" ] ] [ text "Add EMQ" ]
                        ]
                    , studyButton
                    ]
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
                    [ "w-full"
                    , "flex-grow"
                    , "bg-white"
                    , "rounded"
                    , "shadow-lg"
                    , "pb-16"
                    , "min-h-screen"
                    , "md:min-h-0"
                    , "md:pb-0"
                    ]
                ]
                content
    in
    case dataNoteWebData of
        Loading ->
            wrap [ div [ tailwind [ "min-h-screen", "flex", "items-center" ] ] [ div [ class "loading" ] [] ] ]

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
                                [ tailwind [ "mb-4", "w-full", "border-b", "border-gray-400", "pb-8" ] ]
                                [ div
                                    [ tailwind [ "w-full" ] ]
                                    [ textarea
                                        [ placeholder "Contribute public comments, queries, requests, mnemonics or extra notes here."
                                        , value model.comment
                                        , required True
                                        , onInput ChangedComment
                                        , rows 5
                                        ]
                                        []
                                    , button
                                        [ onClick ClickedSubmitComment
                                        , tailwind
                                            [ "hover:bg-blue-400"
                                            , "hover:text-white"
                                            , "bg-gray-200"
                                            , "uppercase"
                                            , "text-gray-600"
                                            , "font-bold"
                                            , "text-sm"
                                            , "ml-auto"
                                            , "block"
                                            ]
                                        ]
                                        [ text "Submit" ]
                                    ]
                                ]
                            , div
                                [ id "comments"
                                , tailwind
                                    []
                                ]
                                (List.map viewComment data.comments)
                            ]
            in
            wrap
                [ div
                    [ tailwind
                        [ "flex"
                        ]
                    ]
                    [ button
                        [ onClick (ClickedTab Official)
                        , tailwind
                            [ "flex-grow"
                            , "rounded-none"
                            , "border-0"
                            , "text-xs"
                            , "uppercase"
                            , "font-bold"
                            , "p-3"
                            , "hover:bg-blue-400"
                            , "hover:text-white"
                            ]
                        , classList
                            [ ( "bg-gray-200 text-gray-600", model.tab == Community )
                            , ( "text-blue-500", model.tab == Official )
                            ]
                        ]
                        [ text "Notes" ]
                    , button
                        [ onClick (ClickedTab Community)
                        , tailwind
                            [ "flex-grow"
                            , "rounded-none"
                            , "border-0"
                            , "text-xs"
                            , "uppercase"
                            , "font-bold"
                            , "p-3"
                            , "hover:bg-blue-400"
                            , "hover:text-white"
                            ]
                        , classList
                            [ ( "bg-gray-200 text-gray-600", model.tab == Official )
                            , ( "text-blue-500", model.tab == Community )
                            ]
                        ]
                        [ text "Contributions" ]
                    ]
                , article
                    [ id "note"
                    , tailwind
                        [ "p-8"
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


viewComment : Comment.ReadData -> Html msg
viewComment data =
    div
        [ tailwind
            [ "p-2"
            , "border-b"
            , "border-gray-400"
            , "my-1"
            ]
        ]
        [ div
            [ class "markdown"
            , tailwind
                []
            ]
            (Markdown.toHtml Nothing data.content)
        , div
            [ tailwind
                [ "text-xs"
                , "text-gray-600"
                , "w-full"
                , "text-right"
                , "mb-2"
                ]
            ]
            [ strong [] [ text data.author.username ]
            , text " "
            , text
                (String.join
                    " "
                    [ "on", Datetime.posixToString data.created_at ]
                )
            ]
        ]


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
