module Page.Revise exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra exposing (isJust, isNothing)
import Page.Elements as Elements
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
import Types.Topic as Topic exposing (Topic)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Url.Builder as Builder
import Views.Question exposing (..)



-- Model


type alias Model =
    { session : Session
    , response : WebData (List Int)
    , quantity : Int
    }


type Modal
    = ModalNone
    | ModalQuestion ModalQuestionData



-- Msg


type Msg
    = NoOp
    | ChangedYearLevel String
    | ChangedSpecialty String
    | ChangedTopic String
    | ChangedDomain String
    | ChangedQuantity String
    | ClickedStart
    | GotQuestionList (WebData (List Int))



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , response = NotAsked
      , quantity = 10
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
                newYearLevel =
                    case string of
                        "nothing" ->
                            Nothing

                        value ->
                            value |> unwrap |> YearLevel.fromInt |> Just

                newSession =
                    { session | reviseYearLevel = newYearLevel }
            in
            ( { model | session = newSession }, Session.save newSession )

        ChangedSpecialty string ->
            let
                newSpecialty =
                    case string of
                        "nothing" ->
                            Nothing

                        value ->
                            value |> unwrap |> Specialty.fromInt |> Just

                newSession =
                    { session | reviseSpecialty = newSpecialty }
            in
            ( { model | session = newSession }, Session.save newSession )

        ChangedDomain string ->
            let
                newDomain =
                    case string of
                        "nothing" ->
                            Nothing

                        value ->
                            value |> unwrap |> Domain.fromInt |> Just

                newSession =
                    { session | reviseDomain = newDomain }
            in
            ( { model | session = newSession }, Session.save newSession )

        ChangedTopic string ->
            let
                newTopic =
                    case string of
                        "nothing" ->
                            Nothing

                        value ->
                            value |> unwrap |> Topic.fromInt |> Just

                newSession =
                    { session | reviseTopic = newTopic }
            in
            ( { model | session = newSession }, Session.save newSession )

        ChangedQuantity string ->
            let
                newValue =
                    String.toInt string

                newBounded =
                    case newValue of
                        Just value ->
                            if value < 1 then
                                1

                            else if value > 100 then
                                100

                            else
                                value

                        Nothing ->
                            10
            in
            ( { model | quantity = newBounded }, Cmd.none )

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


makeQueryList : Session -> List Builder.QueryParameter
makeQueryList session =
    let
        yearLevel =
            session.reviseYearLevel
                |> Maybe.map YearLevel.toInt

        specialty =
            session.reviseSpecialty
                |> Maybe.map Specialty.toInt

        domain =
            session.reviseDomain
                |> Maybe.map Domain.toInt

        topic =
            session.reviseTopic
                |> Maybe.map Topic.toInt

        zipped =
            [ ( Builder.int "note__specialty", specialty )
            , ( Builder.int "note__topic", topic )
            , ( Builder.int "domain", domain )
            , ( Builder.int "year_level", yearLevel )
            ]

        mapped =
            zipped
                |> List.map (\( builder, maybeVal ) -> Maybe.map builder maybeVal)

        filtered =
            Maybe.Extra.values mapped
    in
    filtered


getRandomQuestionList : Model -> Request.GetRequest (List Int) Msg
getRandomQuestionList model =
    { endpoint = Request.GetRandomList
    , auth = model.session.auth
    , callback = GotQuestionList
    , returnDecoder = Decode.list Decode.int
    , queryList = Builder.int "quantity" model.quantity :: makeQueryList model.session
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Revise"
    , body = viewBody model
    }


wrapEnumValue : (a -> String) -> Maybe a -> String
wrapEnumValue converter aMaybe =
    case aMaybe of
        Just a ->
            converter a

        Nothing ->
            "nothing"


viewBody : Model -> List (Html Msg)
viewBody model =
    let
        yearLevelSelect =
            select
                [ onInput <| ChangedYearLevel
                , value (model.session.reviseYearLevel |> wrapEnumValue (YearLevel.toInt >> String.fromInt))
                ]
                (option [ value "nothing", selected True ]
                    [ text "All Year Levels" ]
                    :: List.map (YearLevel.option model.session.reviseYearLevel) YearLevel.list
                )

        specialtySelect =
            select
                [ onInput <| ChangedSpecialty
                , value (model.session.reviseSpecialty |> wrapEnumValue (Specialty.toInt >> String.fromInt))
                ]
                (option [ value "nothing", selected (isNothing model.session.reviseSpecialty) ]
                    [ text "All Specialties" ]
                    :: List.map (Specialty.option model.session.reviseSpecialty) Specialty.list
                )

        topicSelect =
            select
                [ onInput <| ChangedTopic
                , value (model.session.reviseTopic |> wrapEnumValue (Topic.toInt >> String.fromInt))
                ]
                (option [ value "nothing", selected (isNothing model.session.reviseTopic) ]
                    [ text "All Topics" ]
                    :: List.map (Topic.option model.session.reviseTopic) Topic.list
                )

        domainSelect =
            select
                [ onInput <| ChangedDomain
                , value (model.session.reviseDomain |> wrapEnumValue (Domain.toInt >> String.fromInt))
                ]
                (option [ value "nothing", selected (isNothing model.session.reviseDomain) ]
                    [ text "All Domains" ]
                    :: List.map (Domain.option model.session.reviseDomain) Domain.list
                )

        startText =
            case model.response of
                Loading ->
                    "Loading"

                _ ->
                    "Start"
    in
    [ Elements.safeCenter
        [ section
            [ tailwind [ "container", "flex", "justify-center", "items-center" ] ]
            [ article
                [ tailwind
                    [ "bg-white"
                    , "md:shadow-lg"
                    , "rounded"
                    , "flex"
                    , "flex-col"
                    , "h-full"
                    , "w-full"
                    , "md:w-auto"
                    ]
                ]
                [ header
                    [ tailwind
                        [ "p-2"
                        , "font-bold"
                        , "uppercase"
                        , "text-blue-500"
                        , "text-sm"
                        , "p-4"
                        , "border-b"
                        , "border-blue-300"
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
                        [ Elements.label "Specialty" "specialty"
                        , specialtySelect
                        ]
                    , div [ class "field" ]
                        [ Elements.label "Topic" "topic"
                        , topicSelect
                        ]
                    , div [ class "field" ]
                        [ Elements.label "Year Level" "year-level"
                        , yearLevelSelect
                        ]
                    , div [ class "field" ]
                        [ Elements.label "Domain" "domain"
                        , domainSelect
                        ]
                    , div [ class "field" ]
                        [ Elements.label "Quantity" "quantity"
                        , input
                            [ type_ "number"
                            , value (String.fromInt model.quantity)
                            , id "quantity"
                            , onInput ChangedQuantity
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "100"
                            ]
                            []
                        ]
                    ]
                , footer
                    [ tailwind
                        [ "px-2"
                        , "py-2"
                        , "flex"
                        , "justify-end"
                        , "border-t"
                        , "border-blue-300"
                        , "font-bold"
                        , "text-blue-500"
                        , "uppercase"
                        , "text-sm"
                        ]
                    ]
                    [ button
                        [ onClick ClickedStart
                        , tailwind
                            [ "border-blue-500"
                            , "border-2"
                            , "bg-white"
                            , "text-blue-500"
                            , "hover:bg-blue-500"
                            , "hover:text-white"
                            , "uppercase"
                            , "text-sm"
                            , "font-bold"
                            ]
                        ]
                        [ text startText ]
                    ]
                ]
            ]
        ]
    ]
