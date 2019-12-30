module Page.Question exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

{-| This page is the main EMQ review page for reviewing questions.

This page may be entered by:

1.  Clicking Study from a Note page when items are due
2.  Clicking Study from a Note page when no items are due
3.  Clicking Study from the Revise page to start a session
4.  Clicking Next Question from this page itself
5.  Accessing a URL e.g. ./questions/1/

-}

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Elements as Elements
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Request as Request
import Types.Session as Session exposing (Session)



-- Model


type alias Model =
    { session : Session }



-- Msg


type Msg
    = NoOp



-- Init


init : Session -> Int -> ( Model, Cmd Msg )
init session questionId =
    ( { session = session }, Cmd.none )



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
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Document Msg
view model =
    { title = ""
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    []

