module Page.Home exposing (Model, Msg, eject, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Secret exposing (baseUrl)
import Types.Credentials exposing (Auth(..))
import Types.Question
import Types.Request
import Types.Session exposing (Session)
import Types.User as User


type alias Model =
    { session : Session
    , stats : Maybe User.Stats
    }


type Msg
    = NoOp
    | ClickedClassicMode
    | ClickedAdventureMode


init : Session -> ( Model, Cmd Msg )
init session =
    case session.auth of
        Guest ->
            ( { session = session
              , stats = Nothing
              }
            , Cmd.none
            )

        User credentials ->
            -- TODO Request stats
            ( { session = session
              , stats = Nothing
              }
            , Cmd.none
            )


eject : Model -> Session
eject model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            ignore

        ClickedClassicMode ->
            ( model, Navigation.pushUrl model.session.key "/#/classic" )

        ClickedAdventureMode ->
            -- TODO
            ignore


view : Model -> Document Msg
view model =
    { title = ""
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [ class "home screen" ]
        [ h1 [ class "title" ] [ text "AORTA" ]
        , div [ class "subtitle" ] [ text "An open revision tool for assessments." ]
        , img [ class "big", src <| baseUrl ++ "/logo.svg" ] []
        , div [ class "buttons" ]
            [ div [ class "button-container" ]
                [ button [ onClick ClickedClassicMode ]
                    [ text "Classic Mode" ]
                ]
            , div [ class "button-container" ]
                [ button [ onClick ClickedAdventureMode ]
                    [ text "Adventure Mode" ]
                ]
            ]
        ]
