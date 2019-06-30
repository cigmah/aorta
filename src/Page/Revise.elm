module Page.Revise exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Domain as Domain exposing (Domain)
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.Styles exposing (tailwind)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Url.Builder as Builder
import Views.Question exposing (..)



-- Model


type alias Model =
    { session : Session
    , response : WebData (List Int)
    }


type Modal
    = ModalNone
    | ModalQuestion ModalQuestionData



-- Msg


type Msg
    = NoOp
    | ChangedYearLevel String
    | ChangedSpecialty String
    | ClickedStart
    | GotQuestionList (WebData (List Int))



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
            ( { model | session = newSession }, Session.save newSession )

        ChangedSpecialty string ->
            let
                newSession =
                    { session | reviseSpecialty = string |> unwrap |> Specialty.fromInt }
            in
            ( { model | session = newSession }, Session.save newSession )

        ClickedStart ->
            case model.response of
                Loading ->
                    ignore

                -- TODO incorporate filters
                _ ->
                    ( { model | response = Loading }, Request.get (getRandomQuestionList model) )

        GotQuestionList webData ->
            case webData of
                Success data ->
                    case data of
                        [] ->
                            ( { model
                                | session =
                                    Session.addMessage session "There are no questions matching these criteria!"
                                , response = NotAsked
                              }
                            , Cmd.none
                            )

                        head :: tail ->
                            ( { model
                                | session =
                                    { session
                                        | test =
                                            Just
                                                { completed = []
                                                , future = tail
                                                , back = Route.toString Route.Revise
                                                }
                                    }
                              }
                            , Navigation.pushUrl
                                session.key
                                (Route.toString <| Route.Question head)
                            )

                _ ->
                    ( { model | response = webData }, Cmd.none )



-- Requests


getRandomQuestionList : Model -> Request.GetRequest (List Int) Msg
getRandomQuestionList model =
    { endpoint = Request.GetRandomList
    , auth = model.session.auth
    , callback = GotQuestionList
    , returnDecoder = Decode.list Decode.int
    , queryList =
        [ Builder.int "note__year_level" (YearLevel.toInt model.session.reviseYearLevel)
        , Builder.int "note__specialty" (Specialty.toInt model.session.reviseSpecialty)
        ]
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Revise"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    let
        yearLevelSelect =
            select
                [ onInput <| ChangedYearLevel
                , value (model.session.reviseYearLevel |> YearLevel.toInt |> String.fromInt)
                ]
                (List.map (YearLevel.option model.session.reviseYearLevel) YearLevel.list)

        specialtySelect =
            select
                [ onInput <| ChangedSpecialty
                , value (model.session.reviseSpecialty |> Specialty.toInt |> String.fromInt)
                ]
                (List.map (Specialty.option model.session.reviseSpecialty) Specialty.list)
    in
    [ main_
        [ tailwind
            [ "min-h-screen"
            , "flex"
            , "justify-center"
            , "items-center"
            , "w-full"
            , "bg-blue-400"
            , "overflow-auto"
            ]
        ]
        [ section
            [ tailwind [ "container", "flex", "justify-center", "items-center" ] ]
            [ article
                [ tailwind
                    [ "bg-white"
                    , "shadow-lg"
                    , "rounded"
                    , "flex"
                    , "flex-col"
                    , "h-full"
                    ]
                ]
                [ header
                    [ tailwind
                        [ "p-2"
                        ]
                    ]
                    [ text "Revise Random Questions" ]
                , section
                    [ tailwind
                        [ "p-4"
                        , "flex-grow"
                        ]
                    ]
                    [ div [ class "field" ]
                        [ label [] [ text "Year Level" ]
                        , yearLevelSelect
                        ]
                    , div [ class "field" ]
                        [ label [] [ text "Specialty" ]
                        , specialtySelect
                        ]
                    ]
                , footer
                    [ tailwind
                        [ "px-2"
                        , "py-1"
                        , "flex"
                        , "justify-end"
                        ]
                    ]
                    [ button [ onClick ClickedStart ] [ text "Start" ] ]
                ]
            ]
        ]
    ]
