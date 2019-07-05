module Page.Home exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route
import Browser exposing (Document)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Page.Elements as Elements
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Note as Note
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.Styles exposing (tailwind)
import Types.Topic as Topic exposing (Topic)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Url.Builder as Builder



-- Model


type alias Model =
    { session : Session
    }



-- Msg


type Msg
    = NoOp
    | GotNoteList (WebData (List Note.ListData))
    | ChangedFilter String
    | GotSearchResults (WebData (List Note.ListData))



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    let
        initialModel =
            { session = session
            }
    in
    ( initialModel
    , Request.get (getNoteList initialModel)
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
update msg ({ session } as model) =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            ignore

        GotNoteList webData ->
            ( { model | session = { session | webDataNoteList = webData } }, Cmd.none )

        ChangedFilter string ->
            -- At some point, may need to change this if it hits the backend too hard
            let
                cmd =
                    if String.length string > 3 then
                        Request.get (getFilteredNoteList model string)

                    else
                        Cmd.none

                results =
                    if String.length string > 3 then
                        Loading

                    else
                        NotAsked
            in
            ( { model
                | session =
                    { session
                        | searchInput = string
                        , searchResults = results
                    }
              }
            , cmd
            )

        GotSearchResults results ->
            ( { model | session = { session | searchResults = results } }
            , Cmd.none
            )



-- Requests


getNoteList : Model -> Request.GetRequest (List Note.ListData) Msg
getNoteList model =
    { auth = model.session.auth
    , endpoint = Request.GetNoteList
    , callback = GotNoteList
    , returnDecoder = Note.decoderList
    , queryList = []
    }


getFilteredNoteList : Model -> String -> Request.GetRequest (List Note.ListData) Msg
getFilteredNoteList model string =
    { auth = model.session.auth
    , endpoint = Request.GetNoteList
    , callback = GotSearchResults
    , returnDecoder = Note.decoderList
    , queryList = [ Builder.string "search" string ]
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Home"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    let
        searchBarData =
            { webDataResponse = model.session.searchResults
            , responseDataToResult = Note.toSearchResult
            , forMobile = True
            , inputData =
                { value = model.session.searchInput
                , onInput = ChangedFilter
                , placeholder = "Search notes..."
                , type_ = "search"
                }
            }
    in
    [ Elements.safeCenter
        [ viewGrid model.session.webDataNoteList
        , Elements.searchBar searchBarData
        ]
    ]


viewGrid : WebData (List Note.ListData) -> Html Msg
viewGrid webData =
    case webData of
        NotAsked ->
            Elements.errorMessage <|
                p []
                    [ text "The request wasn't made - try refreshing." ]

        Loading ->
            Elements.loadingGrid

        Failure e ->
            Elements.wrapError e

        Success listData ->
            case listData of
                [] ->
                    Elements.errorMessage <|
                        p []
                            [ text "There weren't any notes on the server to show." ]

                nonEmptyData ->
                    Elements.noteGrid nonEmptyData
