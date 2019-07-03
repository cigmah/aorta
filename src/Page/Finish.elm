module Page.Finish exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Styles exposing (tailwind)



-- Model


type alias Model =
    { session : Session }



-- Msg


type Msg
    = NoOp
    | ClickedFinish



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
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
update msg ({ session } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClickedFinish ->
            case session.test of
                Just test ->
                    ( { model | session = { session | test = Nothing } }
                    , Navigation.pushUrl
                        session.key
                        test.back
                    )

                Nothing ->
                    ( model
                    , Navigation.pushUrl
                        session.key
                        (Route.toString Route.Home)
                    )



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Finished!"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ section [ class "modal" ]
        [ viewTestResults model ]
    ]


viewTestResults : Model -> Html Msg
viewTestResults model =
    case model.session.test of
        Just test ->
            article
                []
                [ header [] [ text "Well done!" ]
                , section []
                    [ text "You've successfully completed a batch of "
                    , strong [] [ text (String.fromInt (List.length test.completed)) ]
                    , text " questions - Great job!"
                    ]
                , footer []
                    [ button [ onClick ClickedFinish ] [ text "Finish" ] ]
                ]

        Nothing ->
            article
                []
                [ section [] [ text "It doesn't seem like you were doing a test! Go home?" ] ]
