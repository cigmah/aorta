module Page.Note exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (getAt, setAt)
import Markdown
import Maybe.Extra
import Page.Elements as Elements
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
import Types.Topic as Topic exposing (Topic)
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
    | ChangedYearLevel String
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
                    case String.trim model.comment of
                        "" ->
                            ( { model | session = Session.addMessage session "You need to type a comment first!" }, Cmd.none )

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

                Failure e ->
                    ( { model
                        | webDataComment = webData
                        , session = Session.addMessage session "There was an error with submitting your comment. We apologise - please let us know if it persists!"
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
updateAddQuestion subMsg modalData noteData ({ session } as model) =
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

        ChangedYearLevel string ->
            let
                newYearLevel =
                    string
                        |> String.toInt
                        |> Maybe.withDefault 0
                        |> YearLevel.fromInt
            in
            ( updateQuestion { question | yearLevel = newYearLevel }, Cmd.none )

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
            let
                errors =
                    validateQuestion question
            in
            case modalData.response of
                Loading ->
                    ignore

                _ ->
                    case errors of
                        [] ->
                            ( { model | modal = ModalAddQuestion { modalData | response = Loading } }
                            , Request.post (postQuestion model.session noteData.id question)
                            )

                        _ ->
                            let
                                message =
                                    "Your question was invalid because " ++ String.join ", " errors ++ "."
                            in
                            ( { model | session = Session.addMessage session message }, Cmd.none )

        GotAddQuestionResponse webData ->
            case webData of
                Success _ ->
                    let
                        newQuestion =
                            Question.new
                    in
                    ( { model
                        | modal =
                            ModalAddQuestion
                                { modalData
                                    | response = webData
                                    , question = { newQuestion | yearLevel = question.yearLevel, domain = question.domain }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | modal = ModalAddQuestion { modalData | response = webData } }, Cmd.none )



-- Validator


validateQuestion : Question.CreationData -> List String
validateQuestion question =
    let
        validStem =
            case String.trim question.stem of
                "" ->
                    Just "your stem was blank"

                _ ->
                    Nothing

        content =
            List.map (String.trim << .content) question.choices

        validChoiceContent =
            if List.any ((==) "") content then
                Just "one or more of your choices was blank"

            else
                Nothing

        allUnique =
            let
                unique =
                    List.Extra.unique content
            in
            if List.length unique == List.length content then
                Nothing

            else
                Just "not all your choices were unique"
    in
    Maybe.Extra.values [ validStem, validChoiceContent, allUnique ]



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
    [ Elements.safeMain
        [ Elements.container
            [ viewHeader model model.webDataNote
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


tailwindButton : Attribute msg
tailwindButton =
    -- TODO refactor to css
    tailwind
        [ "flex"
        , "items-center"
        , "p-2"
        , "text-blue-500"
        , "bg-white"
        , "border-2"
        , "border-blue-400"
        , "w-full"
        , "md:rounded"
        , "md:my-1"
        , "hover:bg-blue-400"
        , "hover:text-white"
        , "uppercase"
        , "text-xs"
        , "focus:bg-blue-500"
        , "focus:text-white"
        , "font-bold"
        , "fadein"
        ]


viewHeader : Model -> WebData Note.Data -> Html Msg
viewHeader model dataNoteWebData =
    let
        makeStudyButton studyButtonText =
            button
                [ onClick ClickedStudy
                , tailwindButton
                , tailwind [ "my-2" ]
                ]
                [ i [ class "material-icons" ] [ text "check" ]
                , span [ tailwind [ "ml-2" ] ] [ text studyButtonText ]
                ]

        studyButton =
            case dataNoteWebData of
                Success noteData ->
                    case noteData.dueIds of
                        Just (head :: tail) ->
                            makeStudyButton <| "Review " ++ String.fromInt (List.length tail + 1) ++ " due EMQs"

                        _ ->
                            case noteData.allIds of
                                [] ->
                                    div [] []

                                list ->
                                    makeStudyButton
                                        ("Study all " ++ String.fromInt (List.length list) ++ " EMQs")

                _ ->
                    div [] []

        dueAndKnown =
            case dataNoteWebData of
                Success noteData ->
                    case ( noteData.dueIds, noteData.knownIds ) of
                        ( Just dueIds, Just knownIds ) ->
                            div
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
                                [ text <|
                                    String.fromInt (List.length dueIds)
                                        ++ " due EMQs, "
                                        ++ String.fromInt (List.length knownIds)
                                        ++ " known EMQs."
                                ]

                        _ ->
                            div [] []

                _ ->
                    div [] []

        wrap title maybeSpecialty allIds loading =
            let
                tile =
                    case maybeSpecialty of
                        Nothing ->
                            div [ tailwind [ "md:h-40", "lg:h-56" ] ] []

                        Just specialty ->
                            div
                                [ tailwind
                                    [ "hidden"
                                    , "md:block"
                                    , "relative"
                                    ]
                                ]
                                [ div [ tailwind [] ] [ Specialty.toIcon specialty ]
                                , div
                                    [ tailwind
                                        [ "bg-blue-400"
                                        , "text-white"
                                        , "p-2"
                                        , "text-xs"
                                        , "font-bold"
                                        , "w-full"
                                        ]
                                    ]
                                    [ text title ]
                                ]
            in
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
                    [ tailwind
                        [ "w-full"
                        , "md:w-40"
                        , "bg-blue-500"
                        , "md:bg-white"
                        , "md:border-2"
                        , "md:border-blue-400"
                        , "lg:w-56"
                        , "p-2"
                        , "md:p-0"
                        , "md:rounded-lg"
                        , "transition"
                        , "mx-auto"
                        , "md:mb-2"
                        , "flex"
                        , "md:block"
                        , "items-center"
                        ]
                    ]
                    [ tile
                    , a
                        [ Route.toHref Route.Home
                        , tailwind [ "md:hidden", "text-white", "mr-4", "flex", "items-center" ]
                        ]
                        [ i [ class "material-icons" ] [ text "arrow_back" ] ]
                    , div
                        [ tailwind
                            [ "text-sm", "text-white", "md:text-center", "uppercase", "flex", "items-center", "md:hidden" ]
                        ]
                        [ text title ]
                    ]
                , a
                    [ href "/"
                    , tailwindButton
                    , tailwind [ "hidden", "md:flex" ]
                    ]
                    [ i [ class "material-icons" ] [ text "arrow_back" ]
                    , span [ tailwind [ "ml-2" ] ] [ text "Back" ]
                    ]
                , div
                    [ tailwind
                        [ "bg-gray-200"
                        , "text-gray-600"
                        , "w-full"
                        , "md:my-1"
                        , "text-center"
                        , "font-bold"
                        , "text-sm"
                        , "p-2"
                        , "rounded"
                        , "transition"
                        ]
                    , classList [ ( "opacity-0", loading ) ]
                    ]
                    [ text <| String.fromInt (List.length allIds) ++ " attached EMQs." ]
                , dueAndKnown
                , div
                    [ tailwind [ "flex", "md:block", "w-full" ]
                    ]
                    [ button
                        [ onClick OpenedAddQuestionModal
                        , tailwindButton
                        , tailwind [ "mr-2", "ml-1", "my-2", "md:ml-0", "md:mr-0" ]
                        , classList
                            [ ( "hidden", Session.isGuest model.session )
                            , ( "hidden", loading )
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
                Nothing
                []
                True

        NotAsked ->
            wrap
                "Not Asked"
                Nothing
                []
                False

        Failure e ->
            wrap
                "There was an error."
                Nothing
                []
                False

        Success data ->
            wrap
                data.title
                (Just data.specialty)
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
                    , "md:shadow-lg"
                    , "pb-16"
                    , "min-h-screen"
                    , "md:min-h-0"
                    , "md:pb-0"
                    ]
                ]
                content

        tailwindTab =
            tailwind
                [ "flex-grow"
                , "rounded-none"
                , "border-b-2"
                , "border-gray-200"
                , "text-xs"
                , "uppercase"
                , "font-bold"
                , "p-3"
                , "hover:bg-blue-400"
                , "hover:text-white"
                , "focus:outline-none"
                , "focus:border-blue-400"
                ]
    in
    case dataNoteWebData of
        Loading ->
            wrap [ div [ tailwind [ "h-full", "flex", "items-center" ] ] [ div [ class "loading" ] [] ] ]

        NotAsked ->
            wrap [ text "Not asked" ]

        Failure e ->
            Elements.wrapError e

        Success data ->
            let
                dataContent =
                    case model.tab of
                        Official ->
                            viewNote data.content

                        Community ->
                            let
                                commentView =
                                    case data.comments of
                                        [] ->
                                            [ p [] [ text "There aren't any public contributions yet." ] ]

                                        _ ->
                                            List.map viewComment data.comments

                                submitText =
                                    case model.webDataComment of
                                        Loading ->
                                            "Loading"

                                        _ ->
                                            "Submit"

                                disabledSubmit =
                                    case model.webDataComment of
                                        Loading ->
                                            True

                                        _ ->
                                            False
                            in
                            [ div
                                [ tailwind [ "mb-4", "w-full", "border-b", "border-gray-400", "pb-8" ]
                                , classList [ ( "hidden", Session.isGuest model.session ) ]
                                ]
                                [ div
                                    [ tailwind [ "w-full" ] ]
                                    [ textarea
                                        [ placeholder "Contribute public comments, queries, requests, mnemonics or extra notes here. You can use markdown to format your contribution e.g. *italicised text*, **bolded text**, [link text](https://url.com), ![image mouseover](https://image-url.com), # Heading, ## Subheading etc."
                                        , value model.comment
                                        , required True
                                        , onInput ChangedComment
                                        , rows 5
                                        ]
                                        []
                                    , button
                                        [ onClick ClickedSubmitComment
                                        , tailwindButton
                                        , tailwind [ "justify-center" ]
                                        , disabled disabledSubmit
                                        ]
                                        [ text submitText ]
                                    ]
                                ]
                            , div
                                [ id "comments"
                                ]
                                commentView
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
                        , tailwindTab
                        , classList
                            [ ( "bg-gray-200 text-gray-600", model.tab == Community )
                            , ( "text-blue-500", model.tab == Official )
                            ]
                        ]
                        [ text "Notes" ]
                    , button
                        [ onClick (ClickedTab Community)
                        , tailwindTab
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
            [ text "There are no official AORTA notes yet. In the meantime, you can view public contributions on the contributions tab and can submit contributions if you are logged in." ]

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


viewModal : Modal -> Html Msg
viewModal modal =
    case modal of
        ModalNone ->
            div [] []

        ModalAddQuestion addQuestionData ->
            viewModalAddQuestion addQuestionData


labelTailwind : Attribute msg
labelTailwind =
    tailwind
        [ "uppercase"
        , "text-xs"
        , "font-bold"
        , "text-gray-700"
        ]


viewModalAddQuestion : AddQuestionData -> Html Msg
viewModalAddQuestion addQuestionData =
    let
        viewAddMessage =
            case addQuestionData.response of
                Success question ->
                    div
                        [ tailwind [ "text-green-500", "pl-2", "font-bold", "normal-case", "text-sm", "flex-grow" ] ]
                        [ text "Thank you. Your question was added successfully." ]

                Failure e ->
                    let
                        wrap string =
                            div [ tailwind [ "text-red-500", "pl-2", "font-bold", "normal-case", "text-sm", "flex-grow" ] ] [ text string ]
                    in
                    case e of
                        Http.BadStatus 500 ->
                            wrap "There was an error. Did you fill out all the fields? Let us know!"

                        Http.NetworkError ->
                            wrap "There was a network error."

                        _ ->
                            wrap "There was an error. Let us know!"

                Loading ->
                    div
                        [ tailwind [ "text-grey-500", "pl-2", "font-bold", "normal-case", "text-sm", "flex-grow" ] ]
                        [ text "Loading..." ]

                _ ->
                    div [] []
    in
    section [ class "modal" ]
        [ article []
            [ header
                [ tailwind
                    [ "flex", "items-center", "border-b-2", "border-blue-300" ]
                ]
                [ h1 []
                    [ text "Create an EMQ" ]
                , button
                    [ onClick ClickedCloseModal
                    , tailwind [ "hover:text-blue-800" ]
                    ]
                    [ i [ class "material-icons" ] [ text "close" ] ]
                ]
            , section []
                [ p [ tailwind [ "p-2", "bg-yellow-100", "text-yellow-800", "rounded", "mb-4" ] ]
                    [ text "By submitting this question, you agree that your submission is of your own, original writing and not taken from copyrighted works, including books, past exam papers or proprietary question banks. Any questions found to be in breach of copyright will be removed and offending users may be banned." ]
                , div [ tailwind [ "md:flex", "w-full" ] ]
                    [ div [ class "field", tailwind [ "flex-grow", "md:mr-2" ] ]
                        [ label [ for "year_level", labelTailwind ] [ text "Year Level" ]
                        , select
                            [ onInput (AddQuestionMsg << ChangedYearLevel)
                            , tailwind [ "mb-2" ]
                            , value (addQuestionData.question.yearLevel |> YearLevel.toInt |> String.fromInt)
                            , id "year_level"
                            ]
                            (List.map (YearLevel.option (Just addQuestionData.question.yearLevel)) YearLevel.list)
                        ]
                    , div [ class "field", tailwind [ "flex-grow", "md:ml-2" ] ]
                        [ label [ for "domain", labelTailwind ] [ text "Domain" ]
                        , select
                            [ onInput (AddQuestionMsg << ChangedDomain)
                            , tailwind [ "mb-2" ]
                            , value (addQuestionData.question.domain |> Domain.toInt |> String.fromInt)
                            , id "domain"
                            ]
                            (List.map (Domain.option (Just addQuestionData.question.domain)) Domain.list)
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ for "stem", labelTailwind ] [ text "Question Stem" ]
                    , textarea
                        [ value addQuestionData.question.stem
                        , placeholder "Write your question here. You can format your question with markdown e.g. *italicised text*, **bolded text**, [link text](http://url.com), ![image mouseover](http://image-url.com)"
                        , onInput (AddQuestionMsg << ChangedStem)
                        , id "stem"
                        , required True
                        , rows 5
                        ]
                        []
                    ]
                , div []
                    (List.indexedMap
                        viewCreateChoice
                        addQuestionData.question.choices
                    )
                    |> Html.map AddQuestionMsg
                , div []
                    [ button
                        [ onClick (AddQuestionMsg AddedChoice)
                        , type_ "button"
                        , tailwind
                            [ "border-2"
                            , "border-blue-500"
                            , "hover:bg-blue-500"
                            , "hover:text-white"
                            , "text-blue-500"
                            , "items-center"
                            , "uppercase"
                            , "font-bold"
                            , "text-xs"
                            , "flex"
                            ]
                        ]
                        [ div [ class "material-icons" ] [ text "add" ], text "Add Distractor" ]
                    ]
                ]
            , footer
                [ tailwind [ "border-t-2", "border-blue-300", "flex", "items-center" ] ]
                [ viewAddMessage
                , button
                    [ type_ "submit"
                    , tailwind
                        [ "border-2"
                        , "border-blue-500"
                        , "hover:bg-blue-500"
                        , "hover:text-white"
                        , "text-blue-500"
                        , "font-bold"
                        , "uppercase"
                        , "text-sm"
                        , "ml-2"
                        ]
                    , onClick (AddQuestionMsg PostedQuestion)
                    ]
                    [ text "Add Question" ]
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
                    , labelTailwind
                    , tailwind [ "items-center", "flex" ]
                    ]
                    [ div [ class "material-icons", tailwind [ "mr-2", "text-green-500" ] ] [ text "done" ]
                    , text "Correct Answer"
                    ]
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
                    , placeholder "This is correct because.../Additional explanation"
                    , onInput (ChangedChoiceExplanation int)
                    ]
                    []
                ]

        val ->
            div [ class "field choice" ]
                [ label
                    [ for choiceId
                    , labelTailwind
                    , tailwind [ "items-center", "flex" ]
                    ]
                    [ div [ class "material-icons", tailwind [ "mr-2", "text-red-500" ] ] [ text "clear" ], text "Distractor" ]
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
                    , placeholder "This is incorrect because.../Additional explanation"
                    , onInput (ChangedChoiceExplanation int)
                    , class "incorrect"
                    ]
                    []
                ]
