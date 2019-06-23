module Page.Note exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (getAt, setAt)
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Note as Note
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)



-- TODO Prevent get if loading
-- TODO Read only if not logged in
-- TODO Cap list of choices
-- TODO Allow deleting choices
-- TODO Suppress submit form on add distractor
-- Model


type alias Model =
    { session : Session
    , noteId : Int
    , response : WebData Note.ReadData
    , comment : String
    , commentResponse : WebData Comment.ReadData
    , modal : Modal
    , debugging : WebData Question.ReadData
    }


type Modal
    = NoModal
    | AddQuestionModal Question.CreationData



-- Msg


type Msg
    = NoOp
    | GotNoteData (WebData Note.ReadData)
    | ChangedComment String
    | ClickedSubmitComment
    | GotSubmitCommentResponse (WebData Comment.ReadData)
    | ClickedOpenAddQuestionModal
    | ClickedCloseModal
    | QuestionMsg QuestionSubMsg


type QuestionSubMsg
    = ChangedStem String
    | ChangedCorrectChoiceContent String
    | ChangedCorrectChoiceExplanation String
    | AddedIncorrectChoice
    | ChangedIncorrectChoiceContent Int String
    | ChangedIncorrectChoiceExplanation Int String
    | ClickedSubmitQuestion
    | GotSubmitQuestionResponse (WebData Question.ReadData)



-- Init


init : Session -> Int -> ( Model, Cmd Msg )
init session noteId =
    ( { session = session
      , noteId = noteId
      , response = Loading
      , comment = ""
      , commentResponse = NotAsked
      , modal = NoModal
      , debugging = NotAsked
      }
    , Request.get (getNote session noteId)
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

        GotNoteData webData ->
            ( { model | response = webData }, Cmd.none )

        ChangedComment string ->
            ( { model | comment = string }, Cmd.none )

        ClickedSubmitComment ->
            case model.commentResponse of
                Loading ->
                    ignore

                _ ->
                    ( { model | commentResponse = Loading }
                    , Request.post (postComment model)
                    )

        GotSubmitCommentResponse webData ->
            let
                ( comment, cmd ) =
                    case webData of
                        Success _ ->
                            ( "", Request.get (getNote model.session model.noteId) )

                        _ ->
                            ( model.comment, Cmd.none )
            in
            ( { model | commentResponse = webData, comment = comment }, cmd )

        ClickedOpenAddQuestionModal ->
            ( { model | modal = AddQuestionModal Question.new }, Cmd.none )

        ClickedCloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        QuestionMsg subMsg ->
            case model.modal of
                NoModal ->
                    ignore

                AddQuestionModal data ->
                    updateQuestionMsg subMsg data model


updateQuestionMsg : QuestionSubMsg -> Question.CreationData -> Model -> ( Model, Cmd Msg )
updateQuestionMsg msg data model =
    let
        insert newData =
            ( { model | modal = AddQuestionModal newData }, Cmd.none )

        unwrap process maybeValue =
            case maybeValue of
                Just value ->
                    process value

                Nothing ->
                    ( model, Cmd.none )

        setContentAt int newContent =
            let
                oldChoiceMaybe =
                    getAt int data.choices

                updateOld old =
                    insert { data | choices = setAt int { old | content = newContent } data.choices }
            in
            oldChoiceMaybe
                |> unwrap updateOld

        setExplanationAt int newExplanation =
            let
                oldChoiceMaybe =
                    getAt int data.choices

                updateOld old =
                    insert { data | choices = setAt int { old | explanation = newExplanation } data.choices }
            in
            oldChoiceMaybe
                |> unwrap updateOld
    in
    case msg of
        ChangedStem string ->
            insert { data | stem = string }

        ChangedCorrectChoiceContent string ->
            setContentAt 0 string

        ChangedCorrectChoiceExplanation string ->
            setExplanationAt 0 string

        AddedIncorrectChoice ->
            -- :: would probably be more efficient, but for the moment, it is convenient to add it to the end
            -- for rendering purposes
            if List.length data.choices > 16 then
                ( model, Cmd.none )

            else
                insert { data | choices = List.append data.choices [ Choice.newIncorrect ] }

        ChangedIncorrectChoiceContent int string ->
            setContentAt int string

        ChangedIncorrectChoiceExplanation int string ->
            setExplanationAt int string

        ClickedSubmitQuestion ->
            -- TODO Don't allow if loading
            ( { model | debugging = Loading }
            , Request.post (postQuestion model data) |> Cmd.map QuestionMsg
            )

        GotSubmitQuestionResponse webData ->
            case webData of
                Success question ->
                    ( model
                    , Navigation.pushUrl
                        model.session.key
                        ("./#/questions/" ++ String.fromInt question.id)
                    )

                _ ->
                    ( { model | debugging = webData }, Cmd.none )



-- Requests


getNote : Session -> Int -> Request.GetRequest Note.ReadData Msg
getNote session noteId =
    { endpoint = Request.GetNote noteId
    , auth = session.auth
    , callback = GotNoteData
    , returnDecoder = Note.decoder
    }


postComment : Model -> Request.PostRequest Comment.ReadData Msg
postComment model =
    { endpoint = Request.PostComment
    , auth = model.session.auth
    , callback = GotSubmitCommentResponse
    , returnDecoder = Comment.decoder
    , body =
        Comment.encode
            (Comment.CreationData model.noteId model.comment)
    }


postQuestion : Model -> Question.CreationData -> Request.PostRequest Question.ReadData QuestionSubMsg
postQuestion model data =
    { endpoint = Request.PostQuestion
    , auth = model.session.auth
    , callback = GotSubmitQuestionResponse
    , returnDecoder = Question.decoder
    , body = Question.encode model.noteId data
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Note"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_ []
        [ section []
            [ viewNote model model.response ]
        ]
    , viewAddQuestionModal model
    ]


viewNote : Model -> WebData Note.ReadData -> Html Msg
viewNote model webData =
    case webData of
        NotAsked ->
            text "Not Asked"

        Failure e ->
            text "Failure"

        Loading ->
            text "Loading"

        Success data ->
            article []
                [ header [] [ text data.title ]
                , section []
                    [ section []
                        (Markdown.toHtml Nothing data.content)
                    , section [] (List.map viewComment data.commentList)
                    , viewCommentForm model
                    ]
                , footer []
                    [ button [ onClick ClickedOpenAddQuestionModal ] [ text "Add EMQ" ] ]
                ]


viewCommentForm : Model -> Html Msg
viewCommentForm model =
    Html.form [ onSubmit ClickedSubmitComment ]
        [ section [ class "controls" ]
            [ div [ class "field" ]
                [ label [] [ text "Comment" ]
                , textarea
                    [ value model.comment
                    , onInput ChangedComment
                    , required True
                    , placeholder "Write a comment here."
                    ]
                    []
                ]
            , button [ type_ "submit" ] [ text "Post Comment" ]
            ]
        ]


viewComment : Comment.ReadData -> Html Msg
viewComment data =
    article []
        [ header [] [ text data.author.username ]
        , section [] (Markdown.toHtml Nothing data.content)
        ]


viewAddQuestionModal : Model -> Html Msg
viewAddQuestionModal model =
    case model.modal of
        NoModal ->
            div [] []

        AddQuestionModal data ->
            section [ class "modal" ]
                [ Html.form [ onSubmit (QuestionMsg ClickedSubmitQuestion) ]
                    [ article []
                        [ header [] [ text "Add EMQ" ]
                        , section [ class "controls" ]
                            [ div [ class "field" ]
                                [ label [] [ text "Stem" ]
                                , textarea
                                    [ placeholder "Write your question here."
                                    , value data.stem
                                    , onInput (QuestionMsg << ChangedStem)
                                    ]
                                    []
                                ]
                            , button [ onClick (QuestionMsg AddedIncorrectChoice) ] [ text "Add Distractor" ]
                            , section [] (List.indexedMap viewChoice data.choices) |> Html.map QuestionMsg
                            ]
                        , footer []
                            [ button [ type_ "submit" ] [ text "Add EMQ" ]
                            , button [ onClick ClickedCloseModal ] [ text "Cancel" ]
                            ]
                        ]
                    ]
                ]


viewChoice : Int -> Choice.CreationData -> Html QuestionSubMsg
viewChoice index choice =
    if index == 0 then
        div []
            [ div [ class "field" ]
                [ label [] [ text "Answer" ]
                , input
                    [ type_ "text"
                    , value choice.content
                    , placeholder "Correct Choice"
                    , onInput ChangedCorrectChoiceContent
                    ]
                    []
                ]
            , div [ class "field" ]
                [ label [] [ text "Explanation" ]
                , textarea
                    [ value choice.explanation
                    , placeholder "This is correct because..."
                    , onInput ChangedCorrectChoiceExplanation
                    ]
                    []
                ]
            ]

    else
        div []
            [ div [ class "field" ]
                [ label [] [ text "Distractor" ]
                , input
                    [ type_ "text"
                    , value choice.content
                    , placeholder "Incorrect Choice"
                    , onInput <| ChangedIncorrectChoiceContent index
                    ]
                    []
                ]
            , div [ class "field" ]
                [ label [] [ text "Reasoning" ]
                , textarea
                    [ value choice.explanation
                    , placeholder "This is incorrect because..."
                    , onInput <| ChangedIncorrectChoiceExplanation index
                    ]
                    []
                ]
            ]
