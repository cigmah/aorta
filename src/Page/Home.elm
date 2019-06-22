module Page.Home exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Domain as Domain exposing (Domain)
import Types.Note as Note
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.YearLevel as YearLevel exposing (YearLevel)



-- TODO init based on query parameters
-- Model


type alias Model =
    { session : Session
    , query : String
    , yearLevel : Maybe YearLevel
    , specialty : Maybe Specialty
    , domain : Maybe Domain
    , modal : Modal
    , results : WebData (List Note.ReadData)
    }


type Modal
    = NoModal
    | AddNote Note.CreationData



-- Msg


type Msg
    = NoOp
    | ChangedSearchQuery String
    | ClickedOpenAddNoteModal
    | ClickedCloseModal
    | AddNoteMsg AddNoteSubMsg
    | GotNoteList (WebData (List Note.ReadData))


type AddNoteSubMsg
    = AddChangedYearLevel String
    | AddChangedSpecialty String
    | AddChangedDomain String
    | AddChangedTitle String
    | AddChangedContent String
    | AddClickedSubmit
    | AddGotSubmissionResponse (WebData Note.ReadData)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , query = ""
      , yearLevel = Nothing
      , specialty = Nothing
      , domain = Nothing
      , modal = NoModal
      , results = NotAsked
      }
    , Request.get (getNoteListRequest session)
    )


eject : Model -> Session
eject model =
    model.session


inject : Model -> Session -> ( Model, Cmd Msg )
inject model session =
    ( { model | session = session }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangedSearchQuery query ->
            ( { model | query = query }, Cmd.none )

        ClickedOpenAddNoteModal ->
            ( { model | modal = AddNote Note.new }, Cmd.none )

        ClickedCloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        GotNoteList webData ->
            ( { model | results = webData }, Cmd.none )

        AddNoteMsg subMsg ->
            case model.modal of
                AddNote data ->
                    updateAddNoteMsg subMsg data model

                _ ->
                    ( model, Cmd.none )


updateAddNoteMsg : AddNoteSubMsg -> Note.CreationData -> Model -> ( Model, Cmd Msg )
updateAddNoteMsg addNoteSubMsg data model =
    let
        insert newData =
            ( { model | modal = AddNote newData }, Cmd.none )

        unwrap intString =
            String.toInt intString
                |> Maybe.withDefault 0
    in
    case addNoteSubMsg of
        AddChangedYearLevel string ->
            insert { data | yearLevel = string |> unwrap |> YearLevel.fromInt }

        AddChangedSpecialty string ->
            insert { data | specialty = string |> unwrap |> Specialty.fromInt }

        AddChangedDomain string ->
            insert { data | domain = string |> unwrap |> Domain.fromInt }

        AddChangedTitle new ->
            insert { data | title = new }

        AddChangedContent new ->
            insert { data | content = new }

        AddClickedSubmit ->
            ( model, Request.post <| addNoteRequest model data )

        AddGotSubmissionResponse response ->
            ( model, Cmd.none )



-- Requests


addNoteRequest : Model -> Note.CreationData -> Request.PostRequest Note.ReadData Msg
addNoteRequest model data =
    { endpoint = Request.PostNote
    , body = Note.encode data
    , returnDecoder = Note.decoder
    , callback = AddNoteMsg << AddGotSubmissionResponse
    , auth = model.session.auth
    }


getNoteListRequest : Session -> Request.GetRequest (List Note.ReadData) Msg
getNoteListRequest session =
    { auth = session.auth
    , endpoint = Request.GetNoteList
    , callback = GotNoteList
    , returnDecoder = Note.decoderList
    }



-- View


view : Model -> Document Msg
view model =
    { title = ""
    , body = viewBody model
    }


viewAddNoteButton : Model -> Html Msg
viewAddNoteButton model =
    case model.session.auth of
        Guest ->
            div [] []

        User _ ->
            button [ onClick ClickedOpenAddNoteModal ]
                [ text "Add Note" ]


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_ []
        [ section []
            [ input
                [ type_ "text"
                , placeholder "Search"
                , class "search"
                , onInput ChangedSearchQuery
                , value model.query
                ]
                []
            , button []
                [ text "Search" ]
            , viewAddNoteButton model
            ]
        , section [] (viewResultList model)
        ]
    , viewAddNoteModal model
    ]


viewResultList : Model -> List (Html Msg)
viewResultList model =
    case model.results of
        NotAsked ->
            [ text "Not asked." ]

        Failure e ->
            [ text "Failure" ]

        Success noteList ->
            List.map viewResult noteList

        Loading ->
            [ text "Loading" ]


viewResult : Note.ReadData -> Html Msg
viewResult note =
    article []
        [ header [] [ text note.title ]
        , section [] (Markdown.toHtml Nothing note.content)
        ]


viewAddNoteModal : Model -> Html Msg
viewAddNoteModal model =
    case model.modal of
        NoModal ->
            div [] []

        AddNote data ->
            section [ class "modal" ]
                [ Html.form [ onSubmit <| AddNoteMsg AddClickedSubmit ]
                    [ article []
                        [ header [] [ text "Add Note" ]
                        , section []
                            [ section [ class "controls" ]
                                [ div [ class "field" ]
                                    [ label [] [ text "Year Level" ]
                                    , select
                                        [ onInput <| AddNoteMsg << AddChangedYearLevel ]
                                        (List.map YearLevel.option YearLevel.list)
                                    ]
                                , div [ class "field" ]
                                    [ label [] [ text "Specialty" ]
                                    , select
                                        [ onInput <| AddNoteMsg << AddChangedSpecialty ]
                                        (List.map Specialty.option Specialty.list)
                                    ]
                                , div [ class "field" ]
                                    [ label [] [ text "Domain" ]
                                    , select
                                        [ onInput <| AddNoteMsg << AddChangedDomain ]
                                        (List.map Domain.option Domain.list)
                                    ]
                                , div [ class "field" ]
                                    [ label [] [ text "Title" ]
                                    , input
                                        [ type_ "text"
                                        , value data.title
                                        , onInput <| AddNoteMsg << AddChangedTitle
                                        , placeholder "Title"
                                        , required True
                                        ]
                                        []
                                    ]
                                , div [ class "field" ]
                                    [ label [] [ text "Content" ]
                                    , textarea
                                        [ value data.content
                                        , onInput <| AddNoteMsg << AddChangedContent
                                        , placeholder "Content"
                                        , required True
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , footer []
                            [ button [ onClick ClickedCloseModal ] [ text "Cancel" ]
                            , button [ type_ "submit" ] [ text "Submit" ]
                            ]
                        ]
                    ]
                ]
