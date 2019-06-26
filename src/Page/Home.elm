module Page.Home exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Note as Note
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Url.Builder as Builder



-- Model


type alias Model =
    { session : Session
    , yearLevel : YearLevel
    , webDataNoteList : WebData (List Note.ListData)
    }



-- Msg


type Msg
    = NoOp
    | GotNoteList (WebData (List Note.ListData))
    | ChangedYearLevel YearLevel



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    let
        initialModel =
            { session = session
            , yearLevel = session.yearLevel
            , webDataNoteList = Loading
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
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            ignore

        GotNoteList webData ->
            ( { model | webDataNoteList = webData }, Cmd.none )

        ChangedYearLevel yearLevel ->
            if model.yearLevel /= yearLevel then
                let
                    newSession =
                        Session.changeYearLevel model.session yearLevel

                    newModel =
                        { model
                            | yearLevel = yearLevel
                            , webDataNoteList = Loading
                            , session = newSession
                        }
                in
                ( newModel
                , Cmd.batch
                    [ Request.get (getNoteList newModel)
                    , Session.save newSession
                    ]
                )

            else
                ignore



-- Requests


getNoteList : Model -> Request.GetRequest (List Note.ListData) Msg
getNoteList model =
    { auth = model.session.auth
    , endpoint = Request.GetNoteList
    , callback = GotNoteList
    , returnDecoder = Note.decoderList
    , queryList = [ Builder.int "year_level" (YearLevel.toInt model.yearLevel) ]
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Matrix"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_ []
        [ article []
            [ viewHeader model
            , section []
                [ viewGrid model.webDataNoteList ]
            ]
        ]
    ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "tabs" ]
        (List.map
            (viewHeaderItem model.yearLevel)
            [ YearLevel.Year1, YearLevel.Year2a, YearLevel.Year3b, YearLevel.Year4c ]
        )


viewHeaderItem : YearLevel -> YearLevel -> Html Msg
viewHeaderItem active yearLevel =
    div
        [ classList
            [ ( "active", active == yearLevel ) ]
        , class "tab"
        , onClick (ChangedYearLevel yearLevel)
        ]
        [ text (YearLevel.toString yearLevel) ]


viewGrid : WebData (List Note.ListData) -> Html Msg
viewGrid webData =
    case webData of
        NotAsked ->
            div []
                [ text "Not asked" ]

        Loading ->
            div []
                [ text "Loading" ]

        Failure e ->
            div []
                [ text "Failure" ]

        Success listData ->
            case listData of
                [] ->
                    div []
                        [ text "Oh no! There aren't any notes for this year level yet..." ]

                nonEmptyData ->
                    div [ id "grid" ]
                        (List.map viewGridItem nonEmptyData)


viewGridItem : Note.ListData -> Html Msg
viewGridItem note =
    let
        baseColor =
            Specialty.toColor note.specialty

        hsla =
            Color.toHsla baseColor

        darker =
            { hsla | lightness = 0.3 }

        darkerColor =
            Color.hsl darker.hue darker.saturation darker.lightness
    in
    a
        [ class "note"
        , href ("#/notes/" ++ String.fromInt note.id)
        , Html.Attributes.attribute "data-tooltip" note.title
        ]
        [ div
            [ class "detail"
            , style "background" (Color.toCssString baseColor)
            , style "color" (Color.toCssString darkerColor)
            ]
            [ text note.title ]
        ]
