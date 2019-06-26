module Page.Revise exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Domain as Domain exposing (Domain)
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.YearLevel as YearLevel exposing (YearLevel)



-- Model


type alias Model =
    { session : Session
    , yearLevel : Maybe YearLevel
    , domain : Maybe Domain
    , specialty : Maybe Specialty
    , quantity : Int
    , response : WebData (List Int)
    }



-- Msg


type Msg
    = NoOp
    | ChangedYearLevel String
    | ChangedSpecialty String
    | ChangedDomain String
    | ChangedQuantity String
    | ClickedStartTest
    | GotTest (WebData (List Int))



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , yearLevel = Nothing
      , domain = Nothing
      , specialty = Nothing
      , quantity = 10
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
update msg model =
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
            ( { model | yearLevel = string |> unwrap |> Just << YearLevel.fromInt }, Cmd.none )

        ChangedSpecialty string ->
            ( { model | specialty = string |> unwrap |> Just << Specialty.fromInt }, Cmd.none )

        ChangedDomain string ->
            ( { model | domain = string |> unwrap |> Just << Domain.fromInt }, Cmd.none )

        ChangedQuantity string ->
            ( { model | quantity = string |> unwrap }, Cmd.none )

        ClickedStartTest ->
            case model.response of
                Loading ->
                    ignore

                -- TODO incorporate filters
                _ ->
                    ( { model | response = Loading }, Request.get (getTest model) )

        GotTest webData ->
            ( { model | response = webData }, Cmd.none )



-- Requests


getTest : Model -> Request.GetRequest (List Int) Msg
getTest model =
    { endpoint = Request.GetQuestionListRandom
    , auth = model.session.auth
    , callback = GotTest
    , returnDecoder = Decode.list Decode.int
    }



-- View


view : Model -> Document Msg
view model =
    { title = ""
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
                        , div [ class "field" ]
                            [ label [] [ text "Domain" ]
                            , select
                                [ onInput <| ChangedDomain ]
                                (List.map Domain.option Domain.list)
                            ]
                        , div [ class "field" ]
                            [ label [] [ text "Quantity" ]
                            , input [ type_ "number", value (String.fromInt model.quantity), onInput ChangedQuantity ] []
                            ]
                        ]
                    , footer [] [ button [] [ text "Submit" ] ]
                    ]
                ]
            ]
        ]
    ]
