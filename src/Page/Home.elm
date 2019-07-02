module Page.Home exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route
import Browser exposing (Document)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Note as Note
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.Styles exposing (tailwind)
import Types.Topic as Topic exposing (Topic)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Url.Builder as Builder



-- Model


type alias Model =
    { session : Session
    , webDataNoteList : WebData (List Note.ListData)
    , filter : String
    , results : WebData (List Note.ListData)
    }



-- Msg


type Msg
    = NoOp
    | GotNoteList (WebData (List Note.ListData))
    | ChangedFilter String



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    let
        initialModel =
            { session = session
            , webDataNoteList = Loading
            , filter = ""
            , results = NotAsked
            }
    in
    ( initialModel
    , Request.get (getNoteList initialModel)
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
    in
    case msg of
        NoOp ->
            ignore

        GotNoteList webData ->
            ( { model | webDataNoteList = webData }, Cmd.none )

        ChangedFilter string ->
            ( { model | filter = string }, Cmd.none )



-- Requests


getNoteList : Model -> Request.GetRequest (List Note.ListData) Msg
getNoteList model =
    { auth = model.session.auth
    , endpoint = Request.GetNoteList
    , callback = GotNoteList
    , returnDecoder = Note.decoderList
    , queryList = []
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Matrix"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_
        [ tailwind
            [ "bg-gray-100"
            , "pb-16"
            , "md:pb-0"
            ]
        ]
        [ section
            [ tailwind
                [ "w-full"
                , "md:flex"
                , "md:flex-col"
                , "md:pt-16"
                , "p-2"
                ]
            ]
            [ input
                [ value model.filter
                , onInput ChangedFilter
                , placeholder "Search matrix item titles here."
                , tailwind [ "md:w-1/2", "text-lg", "p-2", "mx-auto" ]
                ]
                []
            , article
                [ tailwind
                    [ "md:container"
                    , "w-full"
                    , "mt-4"
                    ]
                ]
                [ section
                    [ tailwind
                        [ "flex"
                        , "relative"
                        ]
                    ]
                    [ viewGrid model.webDataNoteList ]
                ]
            ]
        ]
    ]


viewGrid : WebData (List Note.ListData) -> Html Msg
viewGrid webData =
    case webData of
        NotAsked ->
            div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                [ text "This is an error that shouldn't happen. If you see it, let us know!" ]

        Loading ->
            div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                [ div [ class "loading" ] [] ]

        Failure e ->
            div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                [ text "Hm, it seems like there was an issue. Try refreshing - if it persists, let us know!" ]

        Success listData ->
            case listData of
                [] ->
                    div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                        [ text "There don't appear to be any notes." ]

                nonEmptyData ->
                    div
                        [ tailwind
                            [ "flex"
                            , "flex-col"
                            , "md:grid"
                            , "mx-auto"
                            ]
                        ]
                        (List.map viewGridItem nonEmptyData)


viewGridItem : Note.ListData -> Html Msg
viewGridItem note =
    a
        [ class "note"
        , tailwind
            [ "md:w-6"
            , "md:h-6"
            , "lg:w-8"
            , "lg:h-8"
            , "my-1"
            , "md:m-px"
            , "rounded"
            , "bg-gray-200"
            , "relative"
            , "p-2"
            , "md:p-0"
            , "transition"
            ]
        , Route.toHref (Route.Note note.id)
        , attribute "data-tooltip" note.title
        , style "grid-column" (String.fromInt (Topic.toInt note.topic + 1))
        , style "grid-row" (String.fromInt (Specialty.toInt note.specialty + 1))
        ]
        [ div
            [ tailwind
                [ "flex", "md:hidden" ]
            ]
            [ div
                [ tailwind
                    [ "bg-gray-400"
                    , "h-24"
                    , "w-24"
                    , "mr-4"
                    , "rounded"
                    ]
                ]
                []
            , div []
                [ h1
                    [ tailwind
                        [ "text-gray-700"
                        , "uppercase"
                        , "font-bold"
                        , "mb-2"
                        ]
                    ]
                    [ text note.title ]
                , div [ tailwind [ "flex", "flex-wrap" ] ]
                    [ div
                        [ tailwind
                            [ "text-xs"
                            , "text-gray-600"
                            , "bg-gray-300"
                            , "px-2"
                            , "rounded-full"
                            , "mr-2"
                            , "mb-2"
                            ]
                        ]
                        [ text (note.specialty |> Specialty.toString) ]
                    , div
                        [ tailwind
                            [ "text-xs"
                            , "text-gray-600"
                            , "bg-gray-300"
                            , "px-2"
                            , "rounded-full"
                            , "mb-2"
                            ]
                        ]
                        [ text (note.topic |> Topic.toString) ]
                    ]
                , div
                    [ tailwind [ "flex", "flex-wrap" ] ]
                    [ div [ tailwind [ "text-xs", "mr-4" ] ] [ text (String.fromInt note.numQuestions ++ " questions.") ]
                    , div [ tailwind [ "text-xs" ] ] [ text (String.fromInt note.numComments ++ " contributions.") ]
                    ]
                ]
            ]
        ]


viewCards : String -> WebData (List Note.ListData) -> List (Html Msg)
viewCards filter webData =
    case webData of
        Loading ->
            [ div [ class "loading" ] [] ]

        NotAsked ->
            []

        Failure e ->
            []

        Success data ->
            data
                |> List.filter (\item -> String.contains (String.toLower filter) (String.toLower item.title))
                |> List.map viewCard


viewCard : Note.ListData -> Html Msg
viewCard note =
    a
        [ Route.toHref (Route.Note note.id)
        , tailwind [ "my-1", "sm:m-2" ]
        ]
        [ article
            [ class "card"
            , tailwind
                [ "text-white"
                , "md:h-40"
                , "w-full"
                , "sm:w-64"
                , "md:w-40"
                , "rounded-lg"
                , "shadow-md"
                , "flex"
                , "flex-col"
                , "cursor-pointer"
                , "hover:shadow-xl"
                , "overflow-hidden"
                , "fadein"
                ]
            ]
            [ section
                [ tailwind
                    [ "font-sm"
                    , "p-2"
                    , "flex-grow"
                    , "font-bold"
                    ]
                ]
                [ text note.title ]
            , footer
                [ tailwind
                    [ "flex"
                    , "p-1"
                    , "flex-wrap"
                    ]
                ]
                [ div [ class "tag" ] [ text (Specialty.toString note.specialty) ]
                , div [ class "tag" ] [ text (String.fromInt note.numComments ++ " comments") ]
                , div [ class "tag" ] [ text (String.fromInt note.numQuestions ++ " questions") ]
                ]
            ]
        ]
