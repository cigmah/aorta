module Page.Home exposing (Model, Msg, eject, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Question
import Types.Request
import Types.Session exposing (Session)


type alias Model =
    { session : Session }


type Msg
    = NoOp


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
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = ""
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [] [ text "Hello!" ]
