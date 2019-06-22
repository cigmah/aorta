module Page.Home exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth)
import Types.Domain as Domain exposing (Domain)
import Types.Note as Note
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.YearLevel as YearLevel exposing (YearLevel)



-- Model


type alias Model =
    { session : Session
    , query : String
    , yearLevel : Maybe YearLevel
    , specialty : Maybe Specialty
    , domain : Maybe Domain
    , modal : Modal
    }


type Modal
    = NoModal
    | AddNote Note.CreationData



-- Msg


type Msg
    = NoOp


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , query = ""
      , yearLevel = Nothing
      , specialty = Nothing
      , domain = Nothing
      , modal = NoModal
      }
    , Cmd.none
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


view : Model -> Document Msg
view model =
    { title = ""
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    []
