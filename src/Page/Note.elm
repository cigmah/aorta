module Page.Note exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Types.Comment as Comment
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Note as Note
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)



-- TODO Prevent get if loading
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
                    [ button [] [ text "Add EMQ" ] ]
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
