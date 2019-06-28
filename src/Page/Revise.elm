module Page.Revise exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Domain as Domain exposing (Domain)
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.YearLevel as YearLevel exposing (YearLevel)



-- Model


type alias Model =
    { session : Session
    , response : WebData Question.ReadData
    }



-- Msg


type Msg
    = NoOp
    | ChangedYearLevel String
    | ChangedSpecialty String
    | ClickedStart
    | GotQuestion (WebData Question.ReadData)



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , response = NotAsked
      }
    , Cmd.none
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

        unwrap intString =
            String.toInt intString
                |> Maybe.withDefault 0
    in
    case msg of
        NoOp ->
            ignore

        ChangedYearLevel string ->
            let
                newSession =
                    { session | reviseYearLevel = string |> unwrap |> YearLevel.fromInt }
            in
            ( { model | session = newSession }, Cmd.none )

        ChangedSpecialty string ->
            let
                newSession =
                    { session | reviseSpecialty = string |> unwrap |> Specialty.fromInt }
            in
            ( { model | session = newSession }, Cmd.none )

        ClickedStart ->
            case model.response of
                Loading ->
                    ignore

                -- TODO incorporate filters
                _ ->
                    ( { model | response = Loading }, Request.get (getRandomQuestion model) )

        GotQuestion webData ->
            ( { model | response = webData }, Cmd.none )



-- Requests


getRandomQuestion : Model -> Request.GetRequest Question.ReadData Msg
getRandomQuestion model =
    { endpoint = Request.GetRandomQuestion
    , auth = model.session.auth
    , callback = GotQuestion
    , returnDecoder = Question.decoder
    , queryList = []
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Revise"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_ []
        [ section []
            [ Html.form []
                [ article []
                    [ header [] [ text "Start Revision Session" ]
                    , section []
                        [ div [ class "field" ]
                            [ label [] [ text "Year Level" ]
                            , select
                                [ onInput <| ChangedYearLevel ]
                                (List.map YearLevel.option YearLevel.list)
                            ]
                        , div [ class "field" ]
                            [ label [] [ text "Specialty" ]
                            , select
                                [ onInput <| ChangedSpecialty ]
                                (List.map Specialty.option Specialty.list)
                            ]
                        ]
                    , footer [] [ button [] [ text "Submit" ] ]
                    ]
                ]
            ]
        ]
    ]
