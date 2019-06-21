module Page.Questions exposing (Model, Msg(..), eject, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Session exposing (Session)


type alias Model =
    { session : Session }


type Msg
    = NoOp
    | Inject Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


eject : Model -> Session
eject model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Inject session ->
            ( { model | session = session }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = ""
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    []
